package org.bosik.diacomp.persistence.services.web.utils.client;

import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.conn.params.ConnManagerParams;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.apache.http.util.EntityUtils;
import org.bosik.diacomp.persistence.services.web.utils.client.exceptions.AuthException;
import org.bosik.diacomp.persistence.services.web.utils.client.exceptions.ConnectionException;
import org.bosik.diacomp.persistence.services.web.utils.client.exceptions.DeprecatedAPIException;
import org.bosik.diacomp.persistence.services.web.utils.client.exceptions.ResponseFormatException;
import org.bosik.diacomp.persistence.services.web.utils.client.exceptions.TaskExecutionException;
import org.bosik.diacomp.persistence.services.web.utils.client.exceptions.UndefinedFieldException;
import org.bosik.diacomp.persistence.services.web.utils.client.exceptions.WebClientException;
import org.bosik.diacomp.utils.Utils;
import org.json.JSONException;
import org.json.JSONObject;
import android.util.Log;

public class WebClient
{
	private static String		TAG						= WebClient.class.getSimpleName();

	/* ================ КОНСТАНТЫ ================ */

	private static final String	API_VERSION				= "1.2";
	private static final String	URL_LOGINPAGE			= "login.php";
	private static final String	URL_CONSOLE				= "console.php";
	private static final String	RESPONSE_UNAUTH			= "Error: log in first";
	private static final String	RESPONSE_DONE			= "DONE";
	private static final String	RESPONSE_FAIL			= "FAIL";
	private static final String	RESPONSE_FAIL_AUTH		= "BADNAME";
	private static final String	RESPONSE_FAIL_APIVER	= "DEPAPI";
	private static final String	RESPONSE_ONLINE			= "online";
	// private static final String RESPONSE_OFFLINE = "offline";
	private static final String	CODEPAGE_CP1251			= "Cp1251";
	private static final String	CODEPAGE_UTF8			= "UTF-8";
	// private static final String SERVER_CODEPAGE = "UTF-8";

	private static final String	CODE_SPACE				= "%20";

	/* ================ ПОЛЯ ================ */

	private HttpClient			mHttpClient				= null;
	private Long				timeShift				= null;
	private boolean				logged					= false;
	private String				username				= "";								// not
																							// null!
	private String				password				= "";
	private String				server					= "";

	private long				lastRequestTime			= 0;
	private static final long	TIME_LIMIT				= 200;

	/* ================ ВСПОМОГАТЕЛЬНЫЕ КЛАССЫ ================ */

	/**
	 * Результат авторизации
	 * 
	 * @author Bosik
	 */
	@Deprecated
	public enum LoginResult
	{
		/**
		 * Одно из полей не определено
		 */
		FAIL_UNDEFIELDS,

		/**
		 * Сервер не отвечает
		 */
		FAIL_CONNECTION,

		/**
		 * Сервер отвечает некорректно
		 */
		FAIL_FORMAT,

		/**
		 * Сервер сообщает об устаревшей версии API
		 */
		FAIL_APIVERSION,

		/**
		 * Сервер сообщает о неправильной паре "логин-пароль"
		 */
		FAIL_AUTH,

		/**
		 * Сервер сообщает об успешной авторизации
		 */
		DONE
	}

	/* ================================ СЛУЖЕБНЫЕ МЕТОДЫ ================================ */

	/**
	 * Преобразует локальное время в серверное. При необходимости пытается предварительно
	 * авторизоваться.
	 * 
	 * @param time
	 *            Локальное время
	 * @return Серверное время
	 */
	public Date localToServer(Date time)
	{
		if (null == time)
		{
			throw new NullPointerException("Specified argument is null");
		}
		if (null == timeShift)
		{
			login();
		}

		/*
		 * if ((timeShift == null) && (login() != LoginResult.DONE)) { throw new
		 * NullPointerException
		 * ("WebDiaryService.localToServer(): TimeShift is null, can't login to fix"); }
		 */

		return new Date(time.getTime() - timeShift);
	}

	/**
	 * Преобразует серверное время в локальное. При необходимости пытается предварительно
	 * авторизоваться.
	 * 
	 * @param time
	 *            Серверное время
	 * @return Локальное время
	 */
	public Date serverToLocal(Date time)
	{
		if (time == null)
		{
			throw new NullPointerException("Specified argument is null");
		}
		if (null == timeShift)
		{
			login();
		}
		/*
		 * if ((timeShift == null) && (login() != LoginResult.DONE)) { throw new
		 * NullPointerException
		 * ("WebDiaryService.serverToLocal(): TimeShift is null, can't login to fix"); }
		 */
		return new Date(time.getTime() + timeShift);
	}

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
		long now = System.currentTimeMillis();
		if ((now - lastRequestTime) < TIME_LIMIT)
		{
			Utils.sleep(TIME_LIMIT - (now - lastRequestTime));
		}

		// Log.i(TAG(), "doGet(), URL='" + URL + "'");
		try
		{
			HttpResponse resp = mHttpClient.execute(new HttpGet(url.replace(" ", CODE_SPACE)));
			return formatResponse(resp, codePage);
		}
		catch (ClientProtocolException e)
		{
			throw new ConnectionException(e);
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
		long now = System.currentTimeMillis();
		if ((now - lastRequestTime) < TIME_LIMIT)
		{
			Utils.sleep(TIME_LIMIT - (now - lastRequestTime));
		}

		// Log.i(TAG(), "doPost(), URL='" + URL + "'");
		try
		{
			HttpEntity entity = new UrlEncodedFormEntity(params, codePage);
			HttpPost post = new HttpPost(url.replace(" ", CODE_SPACE));
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

	private String doGetSmart(String URL, String codePage) throws WebClientException
	{
		String resp = doGet(URL, codePage);

		if (RESPONSE_UNAUTH.equals(resp))
		{
			Log.v(TAG, "doGetSmart(): Session timeout; re-login");
			login();
			return doGet(URL, codePage);
		}
		else
		{
			return resp;
		}
	}

	private String doPostSmart(String URL, List<NameValuePair> params, String codePage) throws WebClientException
	{
		String resp = doPost(URL, params, codePage);

		if (RESPONSE_UNAUTH.equals(resp))
		{
			Log.v(TAG, "doPostSmart(): Session timeout; re-login");
			login();
			return doPost(URL, params, codePage);
		}
		else
		{
			return resp;
		}
	}

	private static void processResponse(String resp)
	{
		try
		{
			JSONObject json = new JSONObject(resp);
			int code = json.getInt("status");
			String msg = json.getString("message");

			switch (code)
			{
			// TODO: use constants

				case 0:
				{
					return;
				}

				// TODO: add another code handlers

				default:
				{
					throw new TaskExecutionException(code, msg);
				}
			}
		}
		catch (JSONException e)
		{
			throw new ResponseFormatException("Invalid JSON respose: " + resp);
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
			logged = false;
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
			logged = false;
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
			logged = false;
		}
	}

	/* ================================ API ================================ */

	/* ---------------------------------- ОБЩЕЕ ---------------------------------- */

	public void login() throws WebClientException
	{
		Log.i(TAG, "login()");
		logged = false;

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
			if (undefLogin)
			{
				Log.e(TAG, "Login is null or empty");
			}
			if (undefPassword)
			{
				Log.e(TAG, "Password is null or empty");
			}
			if (undefServer)
			{
				Log.e(TAG, "Server is null or empty");
			}

			throw new UndefinedFieldException(undefServer, undefLogin, undefPassword);
		}

		// конструируем запрос

		List<NameValuePair> p = new ArrayList<NameValuePair>();
		p.add(new BasicNameValuePair("login", username));
		p.add(new BasicNameValuePair("password", password));
		p.add(new BasicNameValuePair("api", API_VERSION));
		p.add(new BasicNameValuePair("noredir", ""));

		// отправляем

		Date sendedTime = Utils.now();
		String resp = doPost(server + URL_LOGINPAGE, p, CODEPAGE_UTF8);

		if (resp != null)
		{
			Log.d(TAG, "login(): response is " + resp);
			String[] det = resp.split("\\|");

			if (det.length == 2)
			{
				if (det[0].equals(RESPONSE_DONE))
				{
					Log.d(TAG, "login(): response means DONE, parsing time...");
					Date serverTime;
					try
					{
						serverTime = Utils.parseTimeUTC(det[1]);
						timeShift = ((sendedTime.getTime() + Utils.now().getTime()) / 2) - serverTime.getTime();
						// WIN! Если дошли сюда, то всё прошло успешно.
						logged = true;

						Log.d(TAG, "login(): logged OK");
					}
					catch (ParseException e)
					{
						throw new ResponseFormatException("Bad current time format '" + det[1] + "'", e);
					}
				}
				else if (det[0].equals(RESPONSE_FAIL))
				{
					if (det[1].equals(RESPONSE_FAIL_AUTH))
					{
						throw new AuthException("Bad username/password");
					}
					else if (det[1].equals(RESPONSE_FAIL_APIVER))
					{
						throw new DeprecatedAPIException("API " + API_VERSION + " is deprecated");
					}
					else
					{
						throw new ResponseFormatException("Bad format: failed with comment '" + det[1] + "'");
					}
				}
				else
				{
					throw new ResponseFormatException("Bad format: Unknown identificator '" + det[0] + "'");
				}
			}
			else
			{
				String msg = "Bad format: split count != 2; Content:";
				for (int i = 0; i < det.length; i++)
				{
					msg += "\ndet[" + i + "]=" + det[i];
				}
				throw new ResponseFormatException(msg);
			}
		}
		else
		{
			throw new ConnectionException("doPost(): response is null");
		}
	}

	public boolean isOnline(boolean forceUpdate)
	{
		if (forceUpdate)
		{
			logged = false;

			try
			{
				String resp = doGet(server + URL_LOGINPAGE + "?status", CODEPAGE_CP1251);
				logged = RESPONSE_ONLINE.equals(resp);
			}
			catch (WebClientException e)
			{
				return false;
			}
		}

		return logged;
	}

	public boolean isOnline()
	{
		return isOnline(false);
	}

	public void sendMail(String string)
	{
		// конструируем запрос
		List<NameValuePair> p = new ArrayList<NameValuePair>();
		p.add(new BasicNameValuePair("report", ""));
		p.add(new BasicNameValuePair("msg", string));

		// отправляем на сервер
		String resp = doPostSmart(server + URL_CONSOLE, p, CODEPAGE_CP1251);

		// обрабатываем результат
		processResponse(resp);
	}

	/* ------------------------------------- ДНЕВНИК ------------------------------------- */

	public String getModList(String time)
	{
		return doGetSmart(server + WebClient.URL_CONSOLE + "?diary:getModList&time=" + time, CODEPAGE_CP1251);
	}

	public String getPages(List<Date> dates)
	{
		if (dates.isEmpty())
		{
			return "";
		}

		// TODO: optimize if need (use StringBuilder)

		// конструируем запрос
		String query = server + URL_CONSOLE + "?diary:download&format=json&dates=";
		for (Date date : dates)
		{
			query += Utils.formatDate(date) + ",";
		}

		Log.d(TAG, "getPages: query=" + query);

		// обращаемся на сервер
		return doGetSmart(query, CODEPAGE_CP1251);
	}

	/**
	 * Отправляет страницы на сервер.
	 * 
	 * @param pages
	 *            Страницы
	 * @return Успешность отправки
	 */
	public void postPages(String pages)
	{
		if (!pages.equals(""))
		{
			// конструируем запрос
			List<NameValuePair> p = new ArrayList<NameValuePair>();
			p.add(new BasicNameValuePair("diary:upload", ""));
			p.add(new BasicNameValuePair("pages", pages));

			// отправляем на сервер
			String resp = doPostSmart(server + URL_CONSOLE, p, CODEPAGE_CP1251);

			// обрабатываем результат
			processResponse(resp);
		}
	}

	/* ---------------------------------- БАЗА ПРОДУКТОВ ---------------------------------- */

	public int getFoodBaseVersion()
	{
		try
		{
			return Integer.parseInt(doGetSmart(server + URL_CONSOLE + "?foodbase:getVersion", CODEPAGE_CP1251));
		}
		catch (NumberFormatException e)
		{
			throw new ResponseFormatException(e);
		}
	}

	public String getFoodBase()
	{
		/**/long time = System.currentTimeMillis();

		String source = doGetSmart(server + URL_CONSOLE + "?foodbase:download", CODEPAGE_UTF8);

		/**/Log.v(TAG, String.format("FoodBase downloaded in %d msec", System.currentTimeMillis() - time));

		return source;
	}

	public void postFoodBase(int version, String data)
	{
		// конструируем запрос
		List<NameValuePair> p = new ArrayList<NameValuePair>();
		p.add(new BasicNameValuePair("foodbase:upload", ""));
		p.add(new BasicNameValuePair("version", String.valueOf(version)));
		p.add(new BasicNameValuePair("data", data));

		// отправляем на сервер
		String resp = doPostSmart(server + URL_CONSOLE, p, CODEPAGE_UTF8);

		// обрабатываем результат
		processResponse(resp);
	}
}
