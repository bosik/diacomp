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
import org.bosik.diacomp.android.backend.common.webclient.exceptions.ConnectionException;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.ResponseFormatException;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.TaskExecutionException;
import org.bosik.diacomp.android.backend.common.webclient.exceptions.UndefinedFieldException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.utils.Utils;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

public class WebClient
{
	private static final String TAG = WebClient.class.getSimpleName();

	/* ================ CONSTS ================ */

	private static final int    API_VERSION       = 20;
	private static final String ENCODING_UTF8     = "UTF-8";
	private static final String CODE_SPACE        = "%20";
	private static final long   MIN_REQUEST_DELAY = 100 * Utils.NsecPerMsec;            // nsec

	/* ================ FIELDS ================ */

	private final HttpClient mHttpClient;
	private final String     server;
	private       String     username;
	private       String     password;
	private       long       lastRequestTime = 0;

	/* ================================ CONSTRUCTOR ================================ */

	public WebClient(String serverURL, int connectionTimeout)
	{
		this.server = ensureEndsWithSlash(serverURL);
		mHttpClient = new DefaultHttpClient();
		final HttpParams params = mHttpClient.getParams();
		HttpConnectionParams.setConnectionTimeout(params, connectionTimeout);
		HttpConnectionParams.setSoTimeout(params, connectionTimeout);
		ConnManagerParams.setTimeout(params, connectionTimeout);
	}

	/* ================================ ROUTINES ================================ */

	/**
	 * Converts server's response into String
	 *
	 * @param response Server's response
	 * @return String
	 */
	private static String formatResponse(HttpResponse response)
	{
		if (null == response.getEntity())
		{
			throw new ResponseFormatException("Bad response, getEntity() is null");
		}

		try
		{
			String s = EntityUtils.toString(response.getEntity(), ENCODING_UTF8);
			if (s == null)
			{
				throw new ResponseFormatException("Bad response, response is null");
			}

			checkResponseCode(response.getStatusLine().getStatusCode(), s);
			return s;
		}
		catch (IOException e)
		{
			throw new ResponseFormatException(e);
		}
	}

	private static void checkResponseCode(final int code, String msg)
	{
		switch (code)
		{
			case HttpStatus.SC_INTERNAL_SERVER_ERROR:
			{
				throw new TaskExecutionException(code, msg);
			}
			case HttpStatus.SC_NOT_FOUND:
			{
				throw new NotFoundException(msg);
			}
			case HttpStatus.SC_UNAUTHORIZED:
			{
				throw new NotAuthorizedException(msg);
			}
			case HttpStatus.SC_OK:
			default:
			{
				break;
			}
		}
	}

	private synchronized void checkTimeout()
	{
		long now = System.nanoTime();

		if (now - lastRequestTime < MIN_REQUEST_DELAY)
		{
			long sleep = (MIN_REQUEST_DELAY - now + lastRequestTime) / Utils.NsecPerMsec;
			Log.i(TAG, String.format("Too many requests per second, sleeping for %d ms", sleep));
			Utils.sleep(sleep);
		}

		lastRequestTime = now;
	}

	/**
	 * Performs GET request
	 *
	 * @param url
	 * @return
	 */
	private String doGet(String url)
	{
		checkTimeout();

		url = server + url;
		Log.d(TAG, "GET " + url);

		try
		{
			// TODO: check if %20 replacement is necessary
			HttpResponse resp = mHttpClient.execute(new HttpGet(url.replace(" ", CODE_SPACE)));

			return formatResponse(resp);
		}
		catch (IOException e)
		{
			throw new ConnectionException("Failed to GET " + url, e);
		}
	}

	/**
	 * Performs POST request
	 *
	 * @param url
	 * @param params
	 * @return
	 */
	private String doPost(String url, List<NameValuePair> params)
	{
		checkTimeout();

		url = server + url;
		Log.d(TAG, "POST " + url);

		try
		{
			HttpEntity entity = new UrlEncodedFormEntity(params, ENCODING_UTF8);
			HttpPost post = new HttpPost(url.replace(" ", CODE_SPACE));
			post.addHeader(entity.getContentType());
			post.setEntity(entity);
			HttpResponse resp = mHttpClient.execute(post);

			return formatResponse(resp);
		}
		catch (IOException e)
		{
			throw new ConnectionException("Failed to POST " + url, e);
		}
	}

	/**
	 * Performs PUT request
	 *
	 * @param url
	 * @param params
	 * @return
	 */
	private String doPut(String url, List<NameValuePair> params)
	{
		checkTimeout();

		url = server + url;
		Log.d(TAG, "PUT " + url);

		try
		{
			HttpEntity entity = new UrlEncodedFormEntity(params, ENCODING_UTF8);
			HttpPut put = new HttpPut(url.replace(" ", CODE_SPACE));
			put.addHeader(entity.getContentType());
			put.setEntity(entity);
			HttpResponse resp = mHttpClient.execute(put);

			return formatResponse(resp);
		}
		catch (IOException e)
		{
			throw new ConnectionException("Failed to PUT " + url, e);
		}
	}

	private InputStream doLoadStream(String url)
	{
		checkTimeout();

		url = server + url;
		Log.d(TAG, "GET " + url);

		try
		{
			// TODO: check if %20 replacement is necessary
			HttpResponse resp = mHttpClient.execute(new HttpGet(url.replace(" ", CODE_SPACE)));

			if (null == resp.getEntity())
			{
				throw new ResponseFormatException("Bad response, getEntity() is null");
			}

			InputStream stream = resp.getEntity().getContent();
			if (stream == null)
			{
				throw new ResponseFormatException("Bad response, stream is null");
			}

			checkResponseCode(resp.getStatusLine().getStatusCode(), null);
			return stream;
		}
		catch (IOException e)
		{
			throw new ConnectionException("Failed to GET " + url, e);
		}
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
			return doGet(url);
		}
		catch (NotAuthorizedException e)
		{
			login();
			return doGet(url);
		}
	}

	/**
	 * Performs authenticated POST request. Uses default UTF-8 encoding
	 *
	 * @param URL
	 * @param params
	 * @return
	 */
	public String post(String URL, List<NameValuePair> params)
	{
		try
		{
			return doPost(URL, params);
		}
		catch (NotAuthorizedException e)
		{
			login();
			return doPost(URL, params);
		}
	}

	/**
	 * Performs authenticated PUT request. Uses default UTF-8 encoding
	 *
	 * @param URL
	 * @param params
	 * @return
	 */
	public String put(String URL, List<NameValuePair> params)
	{
		try
		{
			return doPut(URL, params);
		}
		catch (NotAuthorizedException e)
		{
			login();
			return doPut(URL, params);
		}
	}

	public InputStream loadStream(String url)
	{
		try
		{
			return doLoadStream(url);
		}
		catch (NotAuthorizedException e)
		{
			login();
			return doLoadStream(url);
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

		List<NameValuePair> p = new ArrayList<>();
		p.add(new BasicNameValuePair("login", username));
		p.add(new BasicNameValuePair("pass", password));
		p.add(new BasicNameValuePair("api", String.valueOf(API_VERSION)));

		// send

		doPost("api/auth/login/", p);
	}
}