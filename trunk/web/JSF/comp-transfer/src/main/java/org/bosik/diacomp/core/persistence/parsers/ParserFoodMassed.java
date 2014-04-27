package org.bosik.diacomp.core.persistence.parsers;

import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.utils.Utils;
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
		json.put("prots", Utils.round2(object.getRelProts()));
		json.put("fats", Utils.round2(object.getRelFats()));
		json.put("carbs", Utils.round2(object.getRelCarbs()));
		json.put("value", Utils.round2(object.getRelValue()));
		json.put("mass", Utils.round2(object.getMass()));

		return json;
	}
}
