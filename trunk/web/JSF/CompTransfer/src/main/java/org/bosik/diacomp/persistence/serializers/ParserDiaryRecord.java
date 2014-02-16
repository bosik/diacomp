package org.bosik.diacomp.persistence.serializers;

import java.util.List;
import org.bosik.diacomp.bo.FoodMassed;
import org.bosik.diacomp.bo.diary.DiaryRecord;
import org.bosik.diacomp.bo.diary.records.BloodRecord;
import org.bosik.diacomp.bo.diary.records.InsRecord;
import org.bosik.diacomp.bo.diary.records.MealRecord;
import org.bosik.diacomp.bo.diary.records.NoteRecord;
import org.bosik.diacomp.utils.Utils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserDiaryRecord extends Parser<DiaryRecord>
{
	private Parser<FoodMassed>	parserFoodMassed	= new ParserFoodMassed();

	@Override
	public DiaryRecord read(JSONObject json) throws JSONException
	{
		String type = json.getString("type");

		if (type.equals("blood"))
		{
			BloodRecord item = new BloodRecord();
			item.setTime(Utils.parseTimeUTC(json.getString("time")));
			item.setValue(json.getDouble("value"));
			item.setFinger(json.getInt("finger"));
			return item;
		}
		else if (type.equals("ins"))
		{
			InsRecord item = new InsRecord();
			item.setTime(Utils.parseTimeUTC(json.getString("time")));
			item.setValue(json.getDouble("value"));
			return item;
		}
		else if (type.equals("meal"))
		{
			MealRecord item = new MealRecord();
			item.setTime(Utils.parseTimeUTC(json.getString("time")));
			item.setShortMeal(json.getBoolean("short"));
			List<FoodMassed> items = parserFoodMassed.readAll(json.getJSONArray("content"));

			for (FoodMassed f : items)
			{
				item.add(f);
			}

			return item;
		}
		else if (type.equals("note"))
		{
			NoteRecord item = new NoteRecord();
			item.setTime(Utils.parseTimeUTC(json.getString("time")));
			item.setText(json.getString("text"));
			return item;
		}
		else
		{
			throw new UnsupportedOperationException("Unknown record type: '" + type + "'");
		}
	}

	@Override
	public JSONObject write(DiaryRecord object) throws JSONException
	{
		// Gson g = new Gson();
		// String jsonStr = g.toJson(object);

		JSONObject json = new JSONObject();

		json.put("time", Utils.formatTimeUTC(object.getTime()));

		if (object.getClass() == BloodRecord.class)
		{
			BloodRecord item = (BloodRecord) object;
			json.put("type", "blood");
			json.put("value", item.getValue());
			json.put("finger", item.getFinger());
		}
		else if (object.getClass() == InsRecord.class)
		{
			InsRecord item = (InsRecord) object;
			json.put("type", "ins");
			json.put("value", item.getValue());
		}
		else if (object.getClass() == MealRecord.class)
		{
			MealRecord item = (MealRecord) object;
			json.put("type", "meal");
			json.put("short", item.getShortMeal());

			JSONArray foods = new JSONArray();
			for (int i = 0; i < item.count(); i++)
			{
				foods.put(parserFoodMassed.write(item.get(i)));
			}
			json.put("content", foods);
		}
		else if (object.getClass() == NoteRecord.class)
		{
			NoteRecord item = (NoteRecord) object;
			json.put("type", "note");
			json.put("text", item.getText());
		}
		else
		{
			throw new UnsupportedOperationException("Unknown record type: " + object.getClass().getCanonicalName());
		}

		return json;
	}
}
