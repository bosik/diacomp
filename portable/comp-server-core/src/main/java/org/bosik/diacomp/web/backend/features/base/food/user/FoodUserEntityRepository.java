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
	int countByIdUserId(int userId);

	int countByIdUserIdAndIdIdStartingWith(int userId, String prefix);

	List<FoodUserEntity> findByIdUserId(int userId);

	List<FoodUserEntity> findByIdUserIdAndDeletedIsFalse(int userId);

	List<FoodUserEntity> findByIdUserIdAndDeletedIsFalseAndNameContainingOrderByName(int userId, String filter);

	List<FoodUserEntity> findByIdUserIdAndDeletedIsFalseAndNameOrderByName(int userId, String exactName);

	FoodUserEntity findByIdUserIdAndIdId(int userId, String id);

	List<FoodUserEntity> findByIdUserIdAndIdIdStartingWith(int userId, String prefix);

	List<FoodUserEntity> findByIdUserIdAndLastModifiedIsGreaterThanEqual(int userId, Date since);

	@Query(value = "SELECT COUNT(1) FROM (SELECT id FROM food_common WHERE id LIKE :prefix% " + "UNION "
			+ "SELECT id FROM food_user WHERE id LIKE :prefix% AND user_id = :userId) AS T", nativeQuery = true)
	int countCombo(@Param("userId") int userId, @Param("prefix") String prefix);

	@Query(value = "SELECT COUNT(1) FROM (SELECT id FROM food_common " + "UNION "
			+ "SELECT id FROM food_user WHERE user_id = :userId) AS T", nativeQuery = true)
	int countCombo(@Param("userId") int userId);
}
