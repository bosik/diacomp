package org.bosik.diacomp.persistence.serializers.fooditem;

import org.bosik.diacomp.bo.foodbase.FoodItem;
import org.bosik.diacomp.fakes.mocks.Mock;
import org.bosik.diacomp.fakes.mocks.MockFoodItem;
import org.bosik.diacomp.persistence.serializers.ParserFoodItem;
import org.bosik.diacomp.persistence.serializers.Serializer;
import org.bosik.diacomp.persistence.serializers.TestSerializer;
import org.bosik.diacomp.persistence.serializers.utils.SerializerAdapter;

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
