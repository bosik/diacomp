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
		item.setRelProts(json.getDouble("prots"));
		item.setRelFats(json.getDouble("fats"));
		item.setRelCarbs(json.getDouble("carbs"));
		item.setRelValue(json.getDouble("value"));
		item.setFromTable(json.getBoolean("table"));
		item.setTag(json.getInt("tag"));

		return item;
	}

	@Override
	public JSONObject write(FoodItem item) throws JSONException
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
}
