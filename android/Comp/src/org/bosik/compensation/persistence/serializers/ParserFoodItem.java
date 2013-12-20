package org.bosik.compensation.persistence.serializers;

import org.bosik.compensation.bo.foodbase.FoodItem;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserFoodItem extends Parser<FoodItem>
{
	@Override
	public FoodItem read(JSONObject json) throws JSONException
	{
		FoodItem item = new FoodItem();

		item.setName(json.getString("name"));
		item.setRelProts(json.getDouble("relProts"));
		item.setRelFats(json.getDouble("relFats"));
		item.setRelCarbs(json.getDouble("relCarbs"));
		item.setRelValue(json.getDouble("relValue"));
		item.setFromTable(json.getBoolean("fromTable"));
		item.setTag(json.getInt("tag"));

		return item;

		// String jsonStr = json.toString();
		// Gson g = new Gson();
		// FoodItem item = g.fromJson(jsonStr, FoodItem.class);
		// return item;
	}

	@Override
	public JSONObject write(FoodItem item) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put("name", item.getName());
		json.put("relProts", item.getRelProts());
		json.put("relFats", item.getRelFats());
		json.put("relCarbs", item.getRelCarbs());
		json.put("relValue", item.getRelValue());
		json.put("fromTable", item.getFromTable());
		json.put("tag", item.getTag());

		return json;

		// Gson g = new Gson();
		// final String s = g.toJson(item);
		// return new JSONObject(s);
	}
}
