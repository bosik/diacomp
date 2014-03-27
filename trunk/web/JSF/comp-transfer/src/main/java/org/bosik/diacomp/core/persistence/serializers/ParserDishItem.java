package org.bosik.diacomp.core.persistence.serializers;

import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.json.JSONException;
import org.json.JSONObject;

public class ParserDishItem extends Parser<DishItem>
{
	@Override
	public DishItem read(JSONObject json) throws JSONException
	{
		// return new Gson().fromJson(json.toString(), DishItem.class);
		// FIXME
		return null;
	}

	@Override
	public JSONObject write(DishItem item) throws JSONException
	{
		// return new JSONObject(new Gson().toJson(item));
		// FIXME
		return null;
	}
}
