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
package org.bosik.diacomp.web.backend.features.base.food.user;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

import java.util.Date;
import java.util.List;

public interface FoodUserEntityRepository extends CrudRepository<FoodUserEntity, String>
{
	int countByUserId(int userId);

	int countByUserIdAndIdStartingWith(int userId, String prefix);

	List<FoodUserEntity> findByUserId(int userId);

	List<FoodUserEntity> findByUserIdAndDeletedIsFalse(int userId);

	List<FoodUserEntity> findByUserIdAndDeletedIsFalseAndNameContainingOrderByName(int userId, String filter);

	List<FoodUserEntity> findByUserIdAndDeletedIsFalseAndNameOrderByName(int userId, String exactName);

	FoodUserEntity findByUserIdAndId(int userId, String id);

	List<FoodUserEntity> findByUserIdAndIdStartingWith(int userId, String prefix);

	List<FoodUserEntity> findByUserIdAndLastModifiedIsGreaterThanEqual(int userId, Date since);

	@Query(value = "SELECT COUNT(1) FROM (SELECT id FROM food_common WHERE id LIKE :prefix% " + "UNION "
			+ "SELECT id FROM food_user WHERE id LIKE :prefix% AND userid = :userId) AS T", nativeQuery = true)
	int countCombo(@Param("userId") int userId, @Param("prefix") String prefix);

	@Query(value = "SELECT COUNT(1) FROM (SELECT id FROM food_common " + "UNION "
			+ "SELECT id FROM food_user WHERE userid = :userId) AS T", nativeQuery = true)
	int countCombo(@Param("userId") int userId);

}
