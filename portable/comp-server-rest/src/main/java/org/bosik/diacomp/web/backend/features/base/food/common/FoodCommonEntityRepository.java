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
package org.bosik.diacomp.web.backend.features.base.food.common;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

import java.util.Date;
import java.util.List;

public interface FoodCommonEntityRepository extends CrudRepository<FoodCommonEntity, String>
{
	List<FoodCommonEntity> findAll();

	@Query(value = "SELECT * FROM food_common WHERE id NOT IN (SELECT id FROM food_user WHERE UserID = :userId)", nativeQuery = true)
	List<FoodCommonEntity> findNotOverridden(@Param("userId") int userId);

	List<FoodCommonEntity> findByDeletedIsFalse();

	List<FoodCommonEntity> findByLastModifiedIsGreaterThanEqual(Date date);

	List<FoodCommonEntity> findByDeletedIsFalseAndNameContains(String filter);

	List<FoodCommonEntity> findByDeletedIsFalseAndName(String name);

	List<FoodCommonEntity> findByIdStartingWith(String prefix);
}