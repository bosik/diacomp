package org.bosik.diacomp.core.persistence.serializers.fooditem;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodItem;
import org.bosik.diacomp.core.persistence.serializers.TestSerializer;
import org.bosik.diacomp.core.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.test.fakes.mocks.MockFoodItem;
import org.bosik.diacomp.core.test.fakes.mocks.MockVersionedConverter;

public class TestSerializerFoodItem extends TestSerializer<Versioned<FoodItem>>
{
	@Override
	protected Mock<Versioned<FoodItem>> getMock()
	{
		return new MockVersionedConverter<FoodItem>(new MockFoodItem());
	}

	@Override
	protected Serializer<Versioned<FoodItem>> getSerializer()
	{
		return new SerializerFoodItem();
	}
}
