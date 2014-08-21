package org.bosik.diacomp.android.backend.common.webclient;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.conn.params.ConnManagerParams;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.apache.http.util.EntityUtils;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.AuthException;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.ConnectionException;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.ResponseFormatException;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.TaskExecutionException;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.UndefinedFieldException;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.WebClientException;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.UnsupportedAPIException;
import org.bosik.diacomp.core.utils.Utils;
import org.json.JSONException;
import android.util.Log;

public class WebClient
{
	private static String		TAG					= WebClient.class.getSimpleName();

	/* ================ КОНСТАНТЫ ================ */

	private static final int	API_VERSION			= 20;
	// TODO: verify if this codepage is necessary
	public static final String	CODEPAGE_CP1251		= "Cp1251";
	public static final String	CODEPAGE_UTF8		= "UTF-8";

	private static final String	CODE_SPACE			= "%20";

	/* ================ ПОЛЯ ================ */

	private HttpClient			mHttpClient			= null;
	private String				username			= "";								// not
																						// null!
	private String				password			= "";
	private String				server				= "";

	private final long			lastRequestTime		= 0;
	private static final long	MIN_REQUEST_DELAY	= 200;

	/* ================================ ЗАПРОСЫ ================================ */

	/**
	 * Преобразует ответ сервера в строку
	 * 
	 * @param resp
	 *            Ответ сервера
	 * @return Строка
	 * @throws ResponseFormatException
	 */
	private static String formatResponse(HttpResponse resp, String codePage) throws ResponseFormatException
	{
		if (resp.getStatusLine().getStatusCode() != HttpStatus.SC_OK)
		{
			// TODO: add error code description
			throw new ResponseFormatException("Bad response, status code is " + resp.getStatusLine().getStatusCode());
		}
		if (null == resp.getEntity())
		{
			throw new ResponseFormatException("Bad response, getEntity() is null");
		}

		try
		{
			return EntityUtils.toString(resp.getEntity(), codePage);
		}
		catch (IOException e)
		{
			throw new ResponseFormatException(e);
		}
	}

	private void checkTimeout()
	{
		long now = System.currentTimeMillis();
		if ((now - lastRequestTime) < MIN_REQUEST_DELAY)
		{
			Log.i(TAG,
					String.format("Too many requests per second, sleeping for %d msec", MIN_REQUEST_DELAY
							- (now - lastRequestTime)));
			Utils.sleep(MIN_REQUEST_DELAY - (now - lastRequestTime));
		}
	}

	/**
	 * Выполняет get-запрос
	 * 
	 * @param url
	 *            Запрашиваемый адрес
	 * @return Ответ сервера
	 * @throws ConnectionException
	 */
	private String doGet(String url, String codePage) throws WebClientException
	{
		checkTimeout();

		// Log.i(TAG(), "doGet(), URL='" + URL + "'");
		try
		{
			url = server + url;
			// TODO: check if %20 replacement is necessary
			HttpResponse resp = mHttpClient.execute(new HttpGet(url.replace(" ", CODE_SPACE)));
			String responseContent = formatResponse(resp, codePage);

			return responseContent;
		}
		catch (IOException e)
		{
			throw new ConnectionException("Failed to request " + url, e);
		}
	}

	/**
	 * Выполняет post-запрос
	 * 
	 * @param url
	 *            Запрашиваемый адрес
	 * @param params
	 *            Параметры
	 * @return Ответ сервера
	 * @throws ConnectionException
	 */
	private String doPost(String url, List<NameValuePair> params, String codePage) throws WebClientException
	{
		checkTimeout();

		try
		{
			HttpEntity entity = new UrlEncodedFormEntity(params, codePage);
			HttpPost post = new HttpPost(server + url.replace(" ", CODE_SPACE));
			post.addHeader(entity.getContentType());
			post.setEntity(entity);
			HttpResponse resp = mHttpClient.execute(post);

			return formatResponse(resp, codePage);
		}
		catch (IOException e)
		{
			throw new ConnectionException("Failed to request " + url, e);
		}
	}

	private String doPut(String url, List<NameValuePair> params, String codePage) throws WebClientException
	{
		checkTimeout();

		try
		{
			HttpEntity entity = new UrlEncodedFormEntity(params, codePage);
			HttpPut put = new HttpPut(server + url.replace(" ", CODE_SPACE));
			put.addHeader(entity.getContentType());
			put.setEntity(entity);
			HttpResponse resp = mHttpClient.execute(put);

			return formatResponse(resp, codePage);
		}
		catch (IOException e)
		{
			throw new ConnectionException("Failed to request " + url, e);
		}
	}

	public StdResponse get(String URL, String codePage) throws WebClientException
	{
		String s = doGet(URL, codePage);
		StdResponse resp = new StdResponse(s);

		if (resp.getCode() == ResponseBuilder.CODE_UNAUTHORIZED)
		{
			login();
			s = doGet(URL, codePage);
			resp = new StdResponse(s);
		}

		checkResponse(resp);
		return resp;
	}

	/**
	 * Uses UTF-8 encoding by default
	 * 
	 * @param URL
	 * @return
	 * @throws WebClientException
	 */
	public StdResponse get(String URL) throws WebClientException
	{
		return get(URL, CODEPAGE_UTF8);
	}

	public StdResponse post(String URL, List<NameValuePair> params, String codePage) throws WebClientException
	{
		String s = doPost(URL, params, codePage);
		StdResponse resp = new StdResponse(s);

		if (resp.getCode() == ResponseBuilder.CODE_UNAUTHORIZED)
		{
			login();
			s = doPost(URL, params, codePage);
			resp = new StdResponse(s);
		}

		checkResponse(resp);
		return resp;
	}

	public StdResponse put(String URL, List<NameValuePair> params, String codePage) throws WebClientException
	{
		String s = doPut(URL, params, codePage);
		StdResponse resp = new StdResponse(s);

		if (resp.getCode() == ResponseBuilder.CODE_UNAUTHORIZED)
		{
			login();
			s = doPut(URL, params, codePage);
			resp = new StdResponse(s);
		}

		checkResponse(resp);
		return resp;
	}

	private static void checkResponse(StdResponse resp) throws CommonServiceException
	{
		switch (resp.getCode())
		{
			case ResponseBuilder.CODE_OK:
				return;
			case ResponseBuilder.CODE_NOTFOUND:
				throw new NotFoundException(null);
			case ResponseBuilder.CODE_UNAUTHORIZED:
				throw new NotAuthorizedException(resp.getResponse());
			case ResponseBuilder.CODE_BADCREDENTIALS:
				throw new NotAuthorizedException(resp.getResponse());
			case ResponseBuilder.CODE_UNSUPPORTED_API:
				throw new UnsupportedAPIException(resp.getResponse());
			case ResponseBuilder.CODE_DEPRECATED_API:
				throw new DeprecatedAPIException(resp.getResponse());
			default: // case ResponseBuilder.CODE_FAIL:
				throw new CommonServiceException("#" + resp.getCode() + ": " + resp.getResponse());
		}
	}

	/* ================================ КОНСТРУКТОР ================================ */

	public WebClient(int connectionTimeout)
	{
		mHttpClient = new DefaultHttpClient();
		final HttpParams params = mHttpClient.getParams();
		HttpConnectionParams.setConnectionTimeout(params, connectionTimeout);
		HttpConnectionParams.setSoTimeout(params, connectionTimeout);
		ConnManagerParams.setTimeout(params, connectionTimeout);
	}

	// =========================== GET / SET ===========================

	public String getUsername()
	{
		return username;
	}

	public void setUsername(String username)
	{
		if (!this.username.equals(username))
		{
			this.username = username;
		}
	}

	public String getPassword()
	{
		return password;
	}

	public void setPassword(String password)
	{
		if (!this.password.equals(password))
		{
			this.password = password;
		}
	}

	public String getServer()
	{
		return server;
	}

	public void setServer(String server)
	{
		if (!server.startsWith("http://"))
		{
			server = "http://" + server;
		}

		if (!server.endsWith("/"))
		{
			server = server + "/";
		}

		if (!this.server.equals(server))
		{
			this.server = server;
		}
	}

	/* ================================ API ================================ */

	/* ---------------------------------- ОБЩЕЕ ---------------------------------- */

	public void login() throws WebClientException
	{
		// проверки

		boolean undefServer = (null == server) || server.equals("");
		boolean undefLogin = (null == username) || username.equals("");
		boolean undefPassword = (null == password) || password.equals("");

		/*
		 * if (null == server) throw new NullPointerException("Server URL can't be null"); if (null
		 * == username) throw new NullPointerException("Username can't be null"); if (null ==
		 * password) throw new NullPointerException("Password can't be null"); if ("" == server)
		 * throw new IllegalArgumentException("Server URL is empty"); if ("" == username) throw new
		 * IllegalArgumentException("Username is empty"); if ("" == password) throw new
		 * IllegalArgumentException("Password is empty");
		 */

		if (undefServer || undefLogin || undefPassword)
		{
			throw new UndefinedFieldException(undefServer, undefLogin, undefPassword);
		}

		// конструируем запрос

		List<NameValuePair> p = new ArrayList<NameValuePair>();
		p.add(new BasicNameValuePair("login", username));
		p.add(new BasicNameValuePair("pass", password));
		p.add(new BasicNameValuePair("api", String.valueOf(API_VERSION)));

		// отправляем

		String resp = doPost("api/auth/login/", p, CODEPAGE_UTF8);

		if (resp != null)
		{
			try
			{
				StdResponse stdResp = new StdResponse(resp);

				switch (stdResp.getCode())
				{
					case ResponseBuilder.CODE_OK:
					{
						break;
					}
					case ResponseBuilder.CODE_BADCREDENTIALS:
					{
						throw new AuthException("Bad username/password");
					}
					// TODO: no 4050 (deprecated, but still supported) handling
					case ResponseBuilder.CODE_UNSUPPORTED_API:
					{
						throw new UnsupportedAPIException(stdResp.getResponse());
					}
					default:
					{
						throw new TaskExecutionException(stdResp.getCode(), "Failed with message "
								+ stdResp.getResponse());
					}
				}
			}
			catch (JSONException e)
			{
				throw new ResponseFormatException("Mailformed response: " + resp, e);
			}
		}
		else
		{
			throw new ConnectionException("doPost(): response is null");
		}
	}

	@Deprecated
	public void sendMail(String string)
	{
		// TODO: move it to mailing service
		throw new UnsupportedOperationException("Sending mail is not implemented yet");
	}
}
