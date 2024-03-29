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
package org.bosik.diacomp.core.persistence.serializers.diary;

import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerDiaryRecord;
import org.bosik.diacomp.core.persistence.serializers.TestSerializer;
import org.bosik.diacomp.core.mocks.Mock;
import org.bosik.diacomp.core.mocks.MockDiaryRecord;
import org.bosik.diacomp.core.mocks.MockVersionedConverter;
import org.bosik.merklesync.Versioned;

public class TestSerializerDiaryRecord extends TestSerializer<Versioned<DiaryRecord>>
{
	@Override
	protected Mock<Versioned<DiaryRecord>> getMock()
	{
		return new MockVersionedConverter<>(new MockDiaryRecord());
	}

	@Override
	protected Serializer<Versioned<DiaryRecord>> getSerializer()
	{
		return new SerializerDiaryRecord();
	}
}
