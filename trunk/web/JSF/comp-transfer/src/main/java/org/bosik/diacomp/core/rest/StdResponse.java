package org.bosik.diacomp.core.rest;

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
		JSONObject json = new JSONObject(s);
		int code = json.getInt("code");
		String msg = json.has("resp") ? json.getString("resp") : "";
		return new StdResponse(code, msg);
	}

	public static String encode(StdResponse resp)
	{
		JSONObject json = new JSONObject();
		json.put("code", resp.getCode());
		json.put("resp", resp.getResponse());
		return json.toString();
	}
}
