/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.persistence.serializers.fooditem;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodItem;
import org.bosik.diacomp.core.persistence.serializers.TestSerializer;
import org.bosik.diacomp.core.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.test.fakes.mocks.MockFoodItem;
import org.bosik.diacomp.core.test.fakes.mocks.MockVersionedConverter;
import org.bosik.merklesync.Versioned;

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
