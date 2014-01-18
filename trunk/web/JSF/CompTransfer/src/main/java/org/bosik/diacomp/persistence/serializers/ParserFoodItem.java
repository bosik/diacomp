package org.bosik.diacomp.persistence.serializers;

import org.bosik.diacomp.bo.foodbase.FoodItem;
import org.json.JSONException;
import org.json.JSONObject;
import com.google.gson.Gson;

public class ParserFoodItem extends Parser<FoodItem>
{
	@Override
	public FoodItem read(JSONObject json) throws JSONException
	{
		return new Gson().fromJson(json.toString(), FoodItem.class);
	}

	@Override
	public JSONObject write(FoodItem item) throws JSONException
	{
		return new JSONObject(new Gson().toJson(item));
	}
}
