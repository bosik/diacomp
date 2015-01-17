package org.bosik.diacomp.core.persistence.parsers;

import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserDishItem extends Parser<DishItem>
{
	private static Parser<FoodMassed>	parserFoodMassed	= new ParserFoodMassed();

	@Override
	public DishItem read(JSONObject json) throws JSONException
	{
		DishItem dish = new DishItem();
		dish.setName(json.getString("name"));
		dish.setTag(json.getInt("tag"));

		if (json.has("mass"))
		{
			dish.setMass(json.getDouble("mass"));
		}
		else
		{
			dish.setMass(null);
		}

		if (json.has("content"))
		{
			JSONArray content = json.getJSONArray("content");
			for (int i = 0; i < content.length(); i++)
			{
				JSONObject item = content.getJSONObject(i);
				FoodMassed foodMassed = parserFoodMassed.read(item);
				dish.add(foodMassed);
			}
		}

		return dish;
	}

	@Override
	public JSONObject write(DishItem item) throws JSONException
	{
		JSONObject json = new JSONObject();

		json.put("name", item.getName());
		json.put("tag", item.getTag());
		if (item.getMass() != null)
		{
			json.put("mass", item.getMass());
		}

		JSONArray content = new JSONArray();
		for (int i = 0; i < item.count(); i++)
		{
			content.put(parserFoodMassed.write(item.get(i)));
		}

		json.put("content", content);

		return json;
	}
}
