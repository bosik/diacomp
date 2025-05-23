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
package org.bosik.diacomp.web.backend.features.diary;

import org.springframework.data.repository.CrudRepository;

import java.util.Date;
import java.util.List;

public interface DiaryEntityRepository extends CrudRepository<DiaryEntity, String>
{
	int countByUserId(int userId);

	int countByUserIdAndIdStartingWith(int userId, String prefix);

	List<DiaryEntity> findByUserId(int userId);

	List<DiaryEntity> findByUserIdOrderByTimeCache(int userId);

	List<DiaryEntity> findByUserIdAndDeletedIsFalse(int userId);

	DiaryEntity findByUserIdAndId(int userId, String id);

	List<DiaryEntity> findByUserIdAndIdStartingWith(int userId, String prefix);

	List<DiaryEntity> findByUserIdAndTimeStampIsGreaterThanEqual(int userId, Date time);

	List<DiaryEntity> findByUserIdAndTimeCacheBetweenOrderByTimeCache(int userId, Date from, Date to);

	List<DiaryEntity> findByUserIdAndTimeCacheBetweenAndDeletedIsFalseOrderByTimeCache(int userId, Date from, Date to);
}
