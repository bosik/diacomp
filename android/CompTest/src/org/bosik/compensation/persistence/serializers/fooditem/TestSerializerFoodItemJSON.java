package org.bosik.compensation.persistence.serializers.fooditem;

import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.serializers.SerializerAdapter;
import org.bosik.compensation.persistence.serializers.Serializer;
import org.bosik.compensation.persistence.serializers.foodbase.ParserFoodItem;

public class TestSerializerFoodItemJSON extends FoodItemSerializerTest
{
	@Override
	protected Serializer<FoodItem> getSerializer()
	{
		return new SerializerAdapter<FoodItem>(new ParserFoodItem());
	}
}
