package org.bosik.compensation.persistence.serializers;

import java.text.ParseException;
import java.util.LinkedList;
import java.util.List;
import org.bosik.compensation.bo.diary.DiaryPage;
import org.bosik.compensation.bo.diary.DiaryRecord;
import org.bosik.compensation.persistence.common.Versioned;
import org.bosik.compensation.persistence.serializers.utils.ParserVersioned;
import org.bosik.compensation.utils.Utils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserDiaryPage extends Parser<DiaryPage>
{
	//private static final String						TAG					= ParserDiaryPage.class.getSimpleName();

	private static Parser<DiaryRecord>				p					= new ParserDiaryRecord();
	private static Parser<Versioned<DiaryRecord>>	parserDiaryRecord	= new ParserVersioned<DiaryRecord>(p);

	@Override
	public DiaryPage read(JSONObject json) throws JSONException
	{
		try
		{
			DiaryPage page = new DiaryPage();

			// data
			JSONArray content = json.getJSONArray("content");
			for (int i = 0; i < content.length(); i++)
			{
				JSONObject jsonItem = content.getJSONObject(i);
				Versioned<DiaryRecord> item = parserDiaryRecord.read(jsonItem);
				page.add(item);
			}

			// header
			page.setDate(Utils.parseDate(json.getString("date")));
			page.setTimeStamp(Utils.parseTimeUTC(json.getString("stamp")));
			page.setVersion(json.getInt("version")); // after reading

			return page;
		}
		catch (ParseException e)
		{
			throw new JSONException(e.getLocalizedMessage());
		}
	}

	@Override
	public JSONObject write(DiaryPage page) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put("date", Utils.formatDate(page.getDate()));
		json.put("stamp", Utils.formatTimeUTC(page.getTimeStamp()));
		json.put("version", page.getVersion());

		List<Versioned<DiaryRecord>> items = new LinkedList<Versioned<DiaryRecord>>();
		for (int i = 0; i < page.count(); i++)
		{
			final Versioned<DiaryRecord> versioned = page.get(i);
			items.add(versioned);
		}

		json.put("content", parserDiaryRecord.writeAll(items));

		return json;
	}
}
