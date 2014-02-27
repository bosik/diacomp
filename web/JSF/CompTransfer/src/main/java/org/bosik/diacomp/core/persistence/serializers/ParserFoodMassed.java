package org.bosik.diacomp.core.persistence.serializers;

import org.bosik.diacomp.core.entities.business.FoodMassed;
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

	private static double round(double x)
	{
		// TODO: seems bad approach
		return ((double) Math.round(x * 100)) / 100;
	}

	@Override
	public JSONObject write(FoodMassed object) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put("name", object.getName());
		json.put("prots", round(object.getRelProts()));
		json.put("fats", round(object.getRelFats()));
		json.put("carbs", round(object.getRelCarbs()));
		json.put("value", round(object.getRelValue()));
		json.put("mass", round(object.getMass()));

		return json;
	}
}
