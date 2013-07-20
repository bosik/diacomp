package org.bosik.compensation.persistence.repository.providers;

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
import org.bosik.compensation.utils.Utils;
import android.util.Log;

public class WebClient
{
	private static String TAG = WebClient.class.getSimpleName();

	/* ================ КОНСТАНТЫ ================ */

	public static final String API_VERSION = "1.2";
	public static final String URL_LOGINPAGE = "login.php";
	public static final String URL_CONSOLE = "console.php";
	public static final String RESPONSE_UNAUTH = "Error: log in first";
	public static final String RESPONSE_DONE = "DONE";
	public static final String RESPONSE_FAIL = "FAIL";
	public static final String RESPONSE_FAIL_AUTH = "BADNAME";
	public static final String RESPONSE_FAIL_APIVER = "DEPAPI";
	public static final String RESPONSE_ONLINE = "online";
	public static final String RESULT_OFFLINE = "offline";
	public static final String SERVER_CODEPAGE = "Cp1251";

	private static final String CODE_SPACE = "%20";

	/* ================ ПОЛЯ ================ */

	private HttpClient mHttpClient = null;
	private Long timeShift = null;
	private boolean logged = false;
	private String username = ""; // not null!
	private String password = "";
	private String server = "";

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

	/**
	 * Ошибка неопределённости одного из полей
	 * 
	 * @author Bosik
	 */
	public static class UndefinedFieldException extends IllegalArgumentException
	{
		private static final long serialVersionUID = 3716509470386883692L;
		public boolean undefServer = false;
		public boolean undefLogin = false;
		public boolean undefPassword = false;

		public UndefinedFieldException(boolean undefServer, boolean undefLogin, boolean undefPassword)
		{
			super("These fields are undefined (empty or null): " + (undefServer ? " server;" : "")
					+ (undefLogin ? " username;" : "") + (undefPassword ? " password;" : ""));

			this.undefServer = undefServer;
			this.undefLogin = undefLogin;
			this.undefPassword = undefPassword;
		}
		// public UndefinedFieldException(Throwable throwable) { super(throwable); }
		// public UndefinedFieldException(String detailMessage, Throwable throwable) {
		// super(detailMessage, throwable); }
	}

	/**
	 * Общая ошибка при работе с сервером
	 * 
	 * @author Bosik
	 */
	public static class ServerException extends RuntimeException
	{
		private static final long serialVersionUID = -4422450897857678241L;

		public ServerException(String detailMessage)
		{
			super(detailMessage);
		}

		public ServerException(Throwable throwable)
		{
			super(throwable);
		}

		public ServerException(String detailMessage, Throwable throwable)
		{
			super(detailMessage, throwable);
		}
	}

	/**
	 * Ошибка установления связи с сервером
	 * 
	 * @author Bosik
	 */
	public static class NoConnectionException extends ServerException
	{
		private static final long serialVersionUID = 5396386468370646791L;

		public NoConnectionException(String detailMessage)
		{
			super(detailMessage);
		}

		public NoConnectionException(Throwable throwable)
		{
			super(throwable);
		}

		public NoConnectionException(String detailMessage, Throwable throwable)
		{
			super(detailMessage, throwable);
		}
	}

	/**
	 * Ошибка формата данных, возвращаемых сервером
	 * 
	 * @author Bosik
	 */
	public static class ResponseFormatException extends ServerException
	{
		private static final long serialVersionUID = 6342429630144198560L;

		public ResponseFormatException(String detailMessage)
		{
			super(detailMessage);
		}

		public ResponseFormatException(Throwable throwable)
		{
			super(throwable);
		}

		public ResponseFormatException(String detailMessage, Throwable throwable)
		{
			super(detailMessage, throwable);
		}
	}

	/**
	 * Ошибка устаревшей версии API клиента
	 * 
	 * @author Bosik
	 */
	public static class DeprecatedAPIException extends ServerException
	{
		private static final long serialVersionUID = -4897188574347397921L;

		/*
		 * public DeprecatedAPIException(String clientApiVersion, String serverApiVersion) {
		 * super("Client API version (" + clientApiVersion +
		 * ") is deprecated, server required version " + serverApiVersion); }
		 */
		public DeprecatedAPIException(String detailMessage)
		{
			super(detailMessage);
		}
	}

	/**
	 * Ошибка авторизации (неверная пара "логин-пароль")
	 * 
	 * @author Bosik
	 */
	public static class AuthException extends ServerException
	{
		private static final long serialVersionUID = 7885618396446513997L;

		public AuthException(String detailMessage)
		{
			super(detailMessage);
		}
	}

	/* ================ СЛУЖЕБНЫЕ МЕТОДЫ ================ */

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
			throw new NullPointerException("Specified argument is null");
		if (null == timeShift)
			login();

		/*
		 * if ((timeShift == null) && (login() != LoginResult.DONE)) { throw new
		 * NullPointerException
		 * ("WebDiaryRepository.localToServer(): TimeShift is null, can't login to fix"); }
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
			throw new NullPointerException("Specified argument is null");
		if (null == timeShift)
			login();
		/*
		 * if ((timeShift == null) && (login() != LoginResult.DONE)) { throw new
		 * NullPointerException
		 * ("WebDiaryRepository.serverToLocal(): TimeShift is null, can't login to fix"); }
		 */
		return new Date(time.getTime() + timeShift);
	}

	/* ================ ЗАПРОСЫ ================ */

	// TODO: методы doGet() / doPost() имеют модификатор public для тестирования, в релизе исправить
	// на private (или пофиг?)

	private static String formatResponse(HttpResponse resp) throws ResponseFormatException
	{
		// TODO: add error code description
		if (resp.getStatusLine().getStatusCode() != HttpStatus.SC_OK)
			throw new ResponseFormatException("Bad response, status code is " + resp.getStatusLine().getStatusCode());
		if (null == resp.getEntity())
			throw new ResponseFormatException("Bad response, getEntity() is null");

		try
		{
			return EntityUtils.toString(resp.getEntity(), SERVER_CODEPAGE);
		} catch (IOException e)
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
	 * @throws NoConnectionException
	 */
	public String doGet(String url) throws ServerException
	{
		// Log.i(TAG(), "doGet(), URL='" + URL + "'");
		try
		{
			HttpResponse resp = mHttpClient.execute(new HttpGet(url.replace(" ", CODE_SPACE)));
			return formatResponse(resp);
		} catch (ClientProtocolException e)
		{
			throw new NoConnectionException(e);
		} catch (IOException e)
		{
			throw new NoConnectionException(e);
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
	 * @throws NoConnectionException
	 */
	public String doPost(String url, List<NameValuePair> params) throws ServerException
	{
		// Log.i(TAG(), "doPost(), URL='" + URL + "'");
		try
		{
			HttpEntity entity = new UrlEncodedFormEntity(params, SERVER_CODEPAGE);
			HttpPost post = new HttpPost(url.replace(" ", CODE_SPACE));
			post.addHeader(entity.getContentType());
			post.setEntity(entity);
			HttpResponse resp = mHttpClient.execute(post);

			return formatResponse(resp);
		} catch (IOException e)
		{
			throw new NoConnectionException(e);
		}
	}

	private boolean kicked(String s)
	{
		return RESPONSE_UNAUTH.equals(s);
	}

	public String doGetSmart(String URL) throws ServerException
	{
		String resp = doGet(URL);

		if (kicked(resp))
		{
			Log.v(TAG, "doGetSmart(): Session timeout; re-login");
			login();
			return doGet(URL);
		} else
		{
			return resp;
		}
	}

	public String doPostSmart(String URL, List<NameValuePair> params) throws ServerException
	{
		String resp = doPost(URL, params);

		if (kicked(resp))
		{
			Log.v(TAG, "doPostSmart(): Session timeout; re-login");
			login();
			return doPost(URL, params);
		} else
		{
			return resp;
		}
	}

	/* ================ КОНСТРУКТОР ================ */

	public WebClient(int timeout)
	{
		mHttpClient = new DefaultHttpClient();
		final HttpParams params = mHttpClient.getParams();
		HttpConnectionParams.setConnectionTimeout(params, timeout);
		HttpConnectionParams.setSoTimeout(params, timeout);
		ConnManagerParams.setTimeout(params, timeout);
	}

	/* ================ API ================ */

	public void login() throws ServerException
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
				Log.e(TAG, "Login is null or empty");
			if (undefPassword)
				Log.e(TAG, "Password is null or empty");
			if (undefServer)
				Log.e(TAG, "Server is null or empty");

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
		String resp = doPost(server + URL_LOGINPAGE, p);

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
						serverTime = Utils.parseTime(det[1]);
						timeShift = (sendedTime.getTime() + Utils.now().getTime()) / 2 - serverTime.getTime();
						// WIN! Если дошли сюда, то всё прошло успешно.
						logged = true;

						Log.d(TAG, "login(): logged OK");
					} catch (ParseException e)
					{
						throw new ResponseFormatException("Bad current time format '" + det[1] + "'", e);
					}
				} else
					if (det[0].equals(RESPONSE_FAIL))
					{
						if (det[1].equals(RESPONSE_FAIL_AUTH))
							throw new AuthException("Bad username/password");
						else
							if (det[1].equals(RESPONSE_FAIL_APIVER))
								throw new DeprecatedAPIException("API " + API_VERSION + " is deprecated");
							else
								throw new ResponseFormatException("Bad format: failed with comment '" + det[1] + "'");
					} else
						throw new ResponseFormatException("Bad format: Unknown identificator '" + det[0] + "'");
			} else
			{
				String msg = "Bad format: split count != 2; Content:";
				for (int i = 0; i < det.length; i++)
				{
					msg += "\ndet[" + i + "]=" + det[i];
				}
				throw new ResponseFormatException(msg);
			}
		} else
			throw new NoConnectionException("doPost(): response is null");
	}

	public boolean isOnline(boolean forceUpdate)
	{
		if (forceUpdate)
		{
			try
			{
				logged = doGet(server + URL_LOGINPAGE + "?status").equals(RESPONSE_ONLINE);
			} catch (ServerException e)
			{
				logged = false;
			}
		}

		return logged;
	}

	public boolean isOnline()
	{
		return isOnline(false);
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
		if (!this.server.equals(server))
		{
			this.server = server;
			logged = false;
		}
	}

	/**
	 * Attempts to authenticate the user credentials on the server.
	 * 
	 * @param username
	 *            The user's name
	 * @param password
	 *            The user's password to be authenticated
	 * @param handler
	 *            The main UI thread's handler instance.
	 * @param context
	 *            The caller Activity's context
	 * @return Thread The thread on which the network mOperations are executed.
	 */
	/*
	 * public Thread backgroundAuth(final String username, final String password, final Handler
	 * handler, final Context context) { final Runnable runnable = new Runnable() { public void
	 * run() { login(username, password, handler, context); } }; // run on background thread. return
	 * performOnBackgroundThread(runnable); }
	 */
}
