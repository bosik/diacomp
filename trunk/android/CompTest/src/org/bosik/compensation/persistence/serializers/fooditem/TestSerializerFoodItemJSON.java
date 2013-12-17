package org.bosik.compensation.persistence.serializers.fooditem;

import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.fakes.mocks.Mock;
import org.bosik.compensation.fakes.mocks.MockFoodItem;
import org.bosik.compensation.persistence.serializers.ParserFoodItem;
import org.bosik.compensation.persistence.serializers.Serializer;
import org.bosik.compensation.persistence.serializers.TestSerializer;
import org.bosik.compensation.persistence.serializers.utils.SerializerAdapter;

public class TestSerializerFoodItemJSON extends TestSerializer<FoodItem>
{
	private static final Mock<FoodItem>					mockFoodItem		= new MockFoodItem();
	private static final SerializerAdapter<FoodItem>	serializerAdapter	= new SerializerAdapter<FoodItem>(
																					new ParserFoodItem());

	@Override
	protected Mock<FoodItem> getMock()
	{
		return mockFoodItem;
	}

	@Override
	protected Serializer<FoodItem> getSerializer()
	{
		return serializerAdapter;
	}
}
