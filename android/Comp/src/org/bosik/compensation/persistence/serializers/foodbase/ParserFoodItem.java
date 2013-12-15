package org.bosik.compensation.persistence.serializers.foodbase;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.serializers.Parser;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserFoodItem implements Parser<FoodItem>
{
	protected FoodItem decodeJson(JSONObject json) throws JSONException, ParseException
	{
		FoodItem item = new FoodItem();

		item.setName(json.getString("name"));
		item.setRelProts(json.getDouble("prots"));
		item.setRelFats(json.getDouble("fats"));
		item.setRelCarbs(json.getDouble("carbs"));
		item.setRelValue(json.getDouble("value"));
		item.setFromTable(json.getBoolean("table"));
		item.setTag(json.getInt("tag"));

		return item;
	}

	protected JSONObject encodeJson(FoodItem item) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put("name", item.getName());
		json.put("prots", item.getRelProts());
		json.put("fats", item.getRelFats());
		json.put("carbs", item.getRelCarbs());
		json.put("value", item.getRelValue());
		json.put("table", item.getFromTable());
		json.put("tag", item.getTag());

		return json;
	}

	@Override
	public FoodItem read(JSONObject json)
	{
		try
		{
			return decodeJson(json);
		}
		catch (JSONException e)
		{
			throw new IllegalArgumentException("Invalid JSON data: " + json, e);
		}
		catch (ParseException e)
		{
			throw new IllegalArgumentException("Invalid JSON data: " + json, e);
		}
	}

	@Override
	public List<FoodItem> readAll(JSONArray json)
	{
		try
		{
			List<FoodItem> list = new ArrayList<FoodItem>();

			for (int i = 0; i < json.length(); i++)
			{
				FoodItem item = decodeJson(json.getJSONObject(i));
				list.add(item);
			}

			return list;
		}
		catch (JSONException e)
		{
			throw new RuntimeException("Invalid JSON data: " + json, e);
		}
		catch (ParseException e)
		{
			throw new IllegalArgumentException("Invalid JSON data: " + json, e);
		}
	}

	@Override
	public JSONObject write(FoodItem item)
	{
		try
		{
			return encodeJson(item);
		}
		catch (JSONException e)
		{
			throw new RuntimeException("Failed to encode JSON", e);
		}
	}

	@Override
	public JSONArray writeAll(List<FoodItem> objects)
	{
		try
		{
			JSONArray json = new JSONArray();
			for (FoodItem item : objects)
			{
				json.put(encodeJson(item));
			}
			return json;
		}
		catch (JSONException e)
		{
			throw new RuntimeException("Failed to encode JSON", e);
		}
	}
}
