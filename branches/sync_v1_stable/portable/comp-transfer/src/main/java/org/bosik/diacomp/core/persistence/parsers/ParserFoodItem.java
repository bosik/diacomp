package org.bosik.diacomp.core.persistence.parsers;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserFoodItem extends Parser<FoodItem>
{
	@Override
	public FoodItem read(JSONObject json) throws JSONException
	{
		//return new Gson().fromJson(json.toString(), FoodItem.class);

		FoodItem item = new FoodItem();

		item.setName(json.getString("name"));
		item.setRelProts(json.getDouble("prots"));
		item.setRelFats(json.getDouble("fats"));
		item.setRelCarbs(json.getDouble("carbs"));
		item.setRelValue(json.getDouble("value"));
		item.setTag(json.getInt("tag"));
		item.setFromTable(json.getBoolean("table"));

		return item;
	}

	@Override
	public JSONObject write(FoodItem object) throws JSONException
	{
		//return new JSONObject(new Gson().toJson(item));

		JSONObject json = new JSONObject();

		json.put("name", object.getName());
		json.put("prots", object.getRelProts());
		json.put("fats", object.getRelFats());
		json.put("carbs", object.getRelCarbs());
		json.put("value", object.getRelValue());
		json.put("tag", object.getTag());
		json.put("table", object.getFromTable());

		return json;
	}
}
