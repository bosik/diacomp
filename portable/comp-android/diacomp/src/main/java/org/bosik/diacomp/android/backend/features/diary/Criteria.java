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
package org.bosik.diacomp.android.backend.features.diary;

import org.bosik.diacomp.android.backend.common.db.tables.TableDiary;
import org.bosik.diacomp.core.utils.Utils;

import java.util.Collections;
import java.util.Date;
import java.util.List;

public interface Criteria
{
	List<String> getClause();

	List<String> getClauseArgs();

	static Criteria excludeDeleted()
	{
		return new Criteria()
		{
			@Override
			public List<String> getClause()
			{
				return Collections.singletonList(String.format("(%s = 0)", TableDiary.COLUMN_DELETED));
			}

			@Override
			public List<String> getClauseArgs()
			{
				return Collections.emptyList();
			}
		};
	}

	static Criteria dateAfter(Date date)
	{
		return new Criteria()
		{
			@Override
			public List<String> getClause()
			{
				return Collections.singletonList(String.format("(%s >= ?)", TableDiary.COLUMN_TIMECACHE));
			}

			@Override
			public List<String> getClauseArgs()
			{
				return Collections.singletonList(Utils.formatTimeUTC(date));
			}
		};
	}
}
