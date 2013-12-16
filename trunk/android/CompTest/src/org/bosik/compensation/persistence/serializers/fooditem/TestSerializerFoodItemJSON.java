package org.bosik.compensation.persistence.serializers.fooditem;

import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.serializers.ParserFoodItem;
import org.bosik.compensation.persistence.serializers.Serializer;
import org.bosik.compensation.persistence.serializers.utils.SerializerAdapter;

public class TestSerializerFoodItemJSON extends FoodItemSerializerTest
{
	@Override
	protected Serializer<FoodItem> getSerializer()
	{
		return new SerializerAdapter<FoodItem>(new ParserFoodItem());
	}
}
