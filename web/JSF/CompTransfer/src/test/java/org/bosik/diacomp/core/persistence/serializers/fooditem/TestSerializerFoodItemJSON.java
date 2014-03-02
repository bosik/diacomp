package org.bosik.diacomp.core.persistence.serializers.fooditem;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.serializers.ParserFoodItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.TestSerializer;
import org.bosik.diacomp.core.persistence.serializers.utils.SerializerAdapter;
import org.bosik.diacomp.core.utils.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.utils.test.fakes.mocks.MockFoodItem;

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
