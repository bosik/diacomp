package org.bosik.compensation.persistence.serializers;

import java.util.List;
import org.bosik.compensation.bo.FoodMassed;
import org.bosik.compensation.bo.diary.DiaryRecord;
import org.bosik.compensation.bo.diary.records.BloodRecord;
import org.bosik.compensation.bo.diary.records.InsRecord;
import org.bosik.compensation.bo.diary.records.MealRecord;
import org.bosik.compensation.bo.diary.records.NoteRecord;
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
			item.setTime(json.getInt("time"));
			item.setValue(json.getDouble("value"));
			item.setFinger(json.getInt("finger"));
			return item;
		}
		else if (type.equals("ins"))
		{
			InsRecord item = new InsRecord();
			item.setTime(json.getInt("time"));
			item.setValue(json.getDouble("value"));
			return item;
		}
		else if (type.equals("meal"))
		{
			MealRecord item = new MealRecord();
			item.setTime(json.getInt("time"));
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
			item.setTime(json.getInt("time"));
			item.setText(json.getString("text"));
			return item;
		}
		else
		{
			throw new UnsupportedOperationException("Record type '" + type + "' is not supported");
		}
	}

	@Override
	public JSONObject write(DiaryRecord object) throws JSONException
	{
		// TODO Auto-generated method stub
		return null;
	}

}
