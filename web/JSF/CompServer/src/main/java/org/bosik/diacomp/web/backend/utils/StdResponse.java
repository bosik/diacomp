package org.bosik.diacomp.web.backend.utils;

import org.json.JSONException;
import org.json.JSONObject;

public class StdResponse
{
	private int		code;
	private String	response;

	public StdResponse(int code, String response)
	{
		this.code = code;
		this.response = response;
	}

	public StdResponse(String s)
	{
		StdResponse decoded = StdResponse.decode(s);
		setCode(decoded.getCode());
		setResponse(decoded.getResponse());
	}

	public int getCode()
	{
		return code;
	}

	public void setCode(int code)
	{
		this.code = code;
	}

	public String getResponse()
	{
		return response;
	}

	public void setResponse(String response)
	{
		this.response = response;
	}

	public static StdResponse decode(String s)
	{
		try
		{
			JSONObject json = new JSONObject(s);
			int code = json.getInt("code");
			String msg = json.has("resp") ? json.getString("resp") : "";
			return new StdResponse(code, msg);
		}
		catch (JSONException e)
		{
			throw new RuntimeException("Invalid JSON: " + s, e);
		}
	}

	public static String encode(StdResponse resp)
	{
		try
		{
			JSONObject json = new JSONObject();
			json.put("code", resp.getCode());
			json.put("resp", resp.getResponse());
			return json.toString();
		}
		catch (JSONException e)
		{
			throw new RuntimeException(e);
		}
	}
}
