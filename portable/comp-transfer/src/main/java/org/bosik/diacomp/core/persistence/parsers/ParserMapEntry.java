package org.bosik.diacomp.core.persistence.parsers;

import java.util.AbstractMap;
import java.util.Map.Entry;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserMapEntry extends Parser<Entry<String, String>>
{
	private static final String	FIELD_KEY	= "key";
	private static final String	FIELD_VALUE	= "value";

	@Override
	public Entry<String, String> read(JSONObject json) throws JSONException
	{
		String key = json.getString(FIELD_KEY);
		String value = json.getString(FIELD_VALUE);
		return new AbstractMap.SimpleEntry<String, String>(key, value);
	}

	@Override
	public JSONObject write(Entry<String, String> object) throws JSONException
	{
		JSONObject json = new JSONObject();
		json.put(FIELD_KEY, object.getKey());
		json.put(FIELD_VALUE, object.getValue());
		return json;
	}
}
