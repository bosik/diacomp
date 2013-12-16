package org.bosik.compensation.persistence.serializers;

import org.bosik.compensation.bo.FoodMassed;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserFoodMassed extends Parser<FoodMassed>
{
	@Override
	public FoodMassed read(JSONObject json) throws JSONException
	{
		FoodMassed item = new FoodMassed();

		item.setName(json.getString("name"));
		item.setRelProts(json.getDouble("prots"));
		item.setRelFats(json.getDouble("fats"));
		item.setRelCarbs(json.getDouble("carbs"));
		item.setRelValue(json.getDouble("value"));
		item.setMass(json.getDouble("mass"));

		return item;
	}

	@Override
	public JSONObject write(FoodMassed object) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put("name", object.getName());
		json.put("prots", object.getRelProts());
		json.put("fats", object.getRelFats());
		json.put("carbs", object.getRelCarbs());
		json.put("value", object.getRelValue());
		json.put("mass", object.getMass());

		return json;
	}
}
