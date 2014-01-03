package org.bosik.diacomp.utils;

import org.json.JSONException;
import org.json.JSONObject;

public class StdResponse
{
	private int				code;
	private String			msg;

	public StdResponse(int code, String msg)
	{
		this.code = code;
		this.msg = msg;
	}

	public int getCode()
	{
		return code;
	}

	public void setCode(int code)
	{
		this.code = code;
	}

	public String getMsg()
	{
		return msg;
	}

	public void setMsg(String msg)
	{
		this.msg = msg;
	}

	public static StdResponse decode(String s)
	{
		try
		{
			JSONObject json = new JSONObject(s);
			int code = json.getInt("code");
			String msg = json.getString("msg");
			return new StdResponse(code, msg);
		}
		catch (JSONException e)
		{
			throw new RuntimeException(e);
		}
	}

	public static String encode(StdResponse resp)
	{
		try
		{
			JSONObject json = new JSONObject();
			json.put("code", resp.getCode());
			json.put("msg", resp.getMsg());
			return json.toString();
		}
		catch (JSONException e)
		{
			throw new RuntimeException(e);
		}
	}
}
