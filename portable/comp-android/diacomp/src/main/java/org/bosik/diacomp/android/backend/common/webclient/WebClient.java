/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package org.bosik.diacomp.android.backend.common.webclient;

import android.util.Log;
import okhttp3.Interceptor.Chain;
import okhttp3.JavaNetCookieJar;
import okhttp3.OkHttpClient;
import okhttp3.ResponseBody;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.ConnectionException;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.TaskExecutionException;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.UndefinedFieldException;
import org.bosik.diacomp.android.backend.common.webclient.retrofit.DiacompApi;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.utils.Utils;
import retrofit2.Response;
import retrofit2.Retrofit;

import java.io.IOException;
import java.io.InputStream;
import java.net.CookieManager;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public class WebClient
{
	private static final String TAG = WebClient.class.getSimpleName();

	/* ================ CONSTS ================ */

	private static final int    API_VERSION       = 20;
	private static final long   MIN_REQUEST_DELAY = 100 * Utils.NsecPerMsec; // nsec
	private static final String API_LOGIN         = "api/auth/login/";

	/* ================ FIELDS ================ */

	private final DiacompApi api;
	private final String     server;
	private       String     username;
	private       String     password;
	private       long       lastRequestTime = 0;

	/* ================================ CONSTRUCTOR ================================ */

	public WebClient(String serverURL, int connectionTimeout)
	{
		this.server = ensureEndsWithSlash(serverURL);
		final Retrofit client = new Retrofit.Builder()
				.baseUrl(this.server)
				.client(new OkHttpClient.Builder()
						.cookieJar(new JavaNetCookieJar(new CookieManager()))
						.connectTimeout(connectionTimeout, TimeUnit.MILLISECONDS)
						.readTimeout(connectionTimeout, TimeUnit.MILLISECONDS)
						.writeTimeout(connectionTimeout, TimeUnit.MILLISECONDS)
						.addInterceptor(this::loggingInterceptor)
						.addInterceptor(this::throttlingInterceptor)
						.addInterceptor(this::responseCodeInterceptor)
						.build())
				.build();

		this.api = client.create(DiacompApi.class);
	}

	/* ================================ INTERCEPTORS ================================ */

	private okhttp3.Response loggingInterceptor(Chain chain) throws IOException
	{
		Log.d(TAG, chain.request().method() + " " + chain.request().url());
		return chain.proceed(chain.request());
	}

	private synchronized okhttp3.Response throttlingInterceptor(Chain chain) throws IOException
	{
		final long now = System.nanoTime();
		if (now - lastRequestTime < MIN_REQUEST_DELAY)
		{
			final long sleep = (MIN_REQUEST_DELAY - now + lastRequestTime) / Utils.NsecPerMsec;
			Log.i(TAG, String.format("Too many requests per second, sleeping for %d ms", sleep));
			Utils.sleep(sleep);
		}

		lastRequestTime = now;
		return chain.proceed(chain.request());
	}

	private okhttp3.Response responseCodeInterceptor(Chain chain) throws IOException
	{
		okhttp3.Response response = chain.proceed(chain.request());

		switch (response.code())
		{
			case 500:
			{
				throw new TaskExecutionException(500, getBody(response.body()));
			}
			case 404:
			{
				throw new NotFoundException(getBody(response.body()));
			}
			case 401:
			{
				if (!chain.request().url().toString().contains(API_LOGIN))
				{
					response.close();
					login();
					response = chain.proceed(chain.request());
				}

				if (response.code() == 401)
				{
					throw new NotAuthorizedException(getBody(response.body()));
				}
			}
			case 200:
			default:
			{
				break;
			}
		}

		return response;
	}

	/* ================================ ROUTINES ================================ */

	private static String getBody(Response<ResponseBody> response) throws IOException
	{
		return getBody(response.body());
	}

	private static String getBody(ResponseBody body) throws IOException
	{
		return body != null
				? body.string()
				: null;
	}

	private static String ensureEndsWithSlash(String url)
	{
		return !url.endsWith("/")
				? url + "/"
				: url;
	}

	// =========================== GET / SET ===========================

	public void setUsername(String username)
	{
		this.username = username;
	}

	public void setPassword(String password)
	{
		this.password = password;
	}

	/* ================================ API ================================ */

	/**
	 * Performs authenticated GET request. Uses default UTF-8 encoding
	 *
	 * @param url
	 * @return
	 */
	public String get(String url)
	{
		try
		{
			return getBody(api.get(url).execute());
		}
		catch (IOException e)
		{
			throw new ConnectionException("Failed to GET " + server + url, e);
		}
	}

	/**
	 * Performs authenticated POST request. Uses default UTF-8 encoding
	 *
	 * @param URL
	 * @param params
	 * @return
	 */
	public String post(String URL, Map<String, String> params)
	{
		try
		{
			return getBody(api.post(URL, params).execute());
		}
		catch (IOException e)
		{
			throw new ConnectionException("Failed to POST " + server + URL, e);
		}
	}

	/**
	 * Performs authenticated PUT request. Uses default UTF-8 encoding
	 *
	 * @param URL
	 * @param params
	 * @return
	 */
	public String put(String URL, Map<String, String> params)
	{
		try
		{
			return getBody(api.put(URL, params).execute());
		}
		catch (IOException e)
		{
			throw new ConnectionException("Failed to PUT " + server + URL, e);
		}
	}

	public InputStream loadStream(String url)
	{
		try
		{
			final Response<ResponseBody> execute = api.get(url).execute();
			return execute.body().byteStream();
		}
		catch (IOException e)
		{
			throw new ConnectionException("Failed to GET " + server + url, e);
		}
	}

	public void login()
	{
		// checks

		boolean undefServer = Utils.isNullOrEmpty(server);
		boolean undefLogin = Utils.isNullOrEmpty(username);
		boolean undefPassword = Utils.isNullOrEmpty(password);

		if (undefServer || undefLogin || undefPassword)
		{
			throw new UndefinedFieldException(undefServer, undefLogin, undefPassword);
		}

		// building request

		final Map<String, String> p = new HashMap<>();
		p.put("login", username);
		p.put("pass", password);
		p.put("api", String.valueOf(API_VERSION));

		// send

		try
		{
			api.post(API_LOGIN, p).execute();
		}
		catch (IOException e)
		{
			throw new ConnectionException("Failed to POST " + server + API_LOGIN, e);
		}
	}
}