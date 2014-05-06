package org.bosik.diacomp.core.persistence.serializers.dishitem;

import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerDishItem;
import org.bosik.diacomp.core.persistence.serializers.TestSerializer;
import org.bosik.diacomp.core.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.test.fakes.mocks.MockDishItem;
import org.bosik.diacomp.core.test.fakes.mocks.MockVersionedConverter;

public class TestSerializerDishItem extends TestSerializer<Versioned<DishItem>>
{
	@Override
	protected Mock<Versioned<DishItem>> getMock()
	{
		return new MockVersionedConverter<DishItem>(new MockDishItem());
	}

	@Override
	protected Serializer<Versioned<DishItem>> getSerializer()
	{
		return new SerializerDishItem();
	}
}
