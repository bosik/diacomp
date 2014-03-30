package org.bosik.diacomp.core.persistence.serializers.dishbase;

import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.persistence.parsers.ParserDishItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.TestSerializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.utils.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.utils.test.fakes.mocks.MockDishItem;

public class TestSerializerDishItemJSON extends TestSerializer<DishItem>
{
	private static final Mock<DishItem>					mockDishItem		= new MockDishItem();
	private static final SerializerAdapter<DishItem>	serializerAdapter	= new SerializerAdapter<DishItem>(
																					new ParserDishItem());

	@Override
	protected Mock<DishItem> getMock()
	{
		return mockDishItem;
	}

	@Override
	protected Serializer<DishItem> getSerializer()
	{
		return serializerAdapter;
	}
}
