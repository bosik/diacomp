package org.bosik.diacomp.core.persistence.parsers;

import org.bosik.diacomp.core.services.preferences.Preference;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserPreferenceEntry extends Parser<PreferenceEntry<String>>
{
	private static final String	FIELD_KEY		= "key";
	private static final String	FIELD_VALUE		= "value";
	private static final String	FIELD_VERSION	= "version";

	@Override
	public PreferenceEntry<String> read(JSONObject json) throws JSONException
	{
		String key = json.getString(FIELD_KEY);
		String value = json.getString(FIELD_VALUE);
		int version = json.getInt(FIELD_VERSION);

		PreferenceEntry<String> entry = new PreferenceEntry<String>();
		entry.setType(Preference.parse(key));
		entry.setValue(value);
		entry.setVersion(version);

		return entry;
	}

	@Override
	public JSONObject write(PreferenceEntry<String> object) throws JSONException
	{
		JSONObject json = new JSONObject();
		json.put(FIELD_KEY, object.getType().getKey());
		json.put(FIELD_VALUE, object.getValue());
		json.put(FIELD_VERSION, object.getVersion());
		return json;
	}
}
