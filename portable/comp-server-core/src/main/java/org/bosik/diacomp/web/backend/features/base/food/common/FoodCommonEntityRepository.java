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
	// Org = original (non-overridden)
	String IS_ORIGINAL = "id NOT IN (SELECT u.key.id FROM FoodUserEntity u WHERE u.key.userId = :userId)";

	List<FoodCommonEntity> findAll();

	@Query(value = "SELECT COUNT(1) FROM FoodCommonEntity f WHERE " + IS_ORIGINAL)
	int countOrg(@Param("userId") int userId);

	@Query(value = "SELECT COUNT(1) FROM FoodCommonEntity f WHERE " + IS_ORIGINAL + " AND id LIKE :prefix%")
	int countOrgByIdStartingWith(@Param("userId") int userId, @Param("prefix") String prefix);

	@Query(value = "SELECT f FROM FoodCommonEntity f WHERE " + IS_ORIGINAL)
	List<FoodCommonEntity> findOrg(@Param("userId") int userId);

	@Query(value = "SELECT f FROM FoodCommonEntity f WHERE " + IS_ORIGINAL + " AND deleted = false")
	List<FoodCommonEntity> findOrgByDeletedIsFalse(@Param("userId") int userId);

	@Query(value = "SELECT f FROM FoodCommonEntity f WHERE " + IS_ORIGINAL + " AND deleted = false AND name LIKE %:filter%")
	List<FoodCommonEntity> findOrgByDeletedIsFalseAndNameContaining(@Param("userId") int userId, @Param("filter") String filter);

	@Query(value = "SELECT f FROM FoodCommonEntity f WHERE " + IS_ORIGINAL + " AND id LIKE :prefix%")
	List<FoodCommonEntity> findOrgByIdStartingWith(@Param("userId") int userId, @Param("prefix") String prefix);

	@Query(value = "SELECT f FROM FoodCommonEntity f WHERE " + IS_ORIGINAL + " AND lastModified >= :since")
	List<FoodCommonEntity> findOrgByLastModifiedIsGreaterThanEqual(@Param("userId") int userId, @Param("since") Date since);

	List<FoodCommonEntity> findByDeletedIsFalse();

	List<FoodCommonEntity> findByLastModifiedIsGreaterThanEqual(Date date);

	List<FoodCommonEntity> findByDeletedIsFalseAndNameContains(String filter);

	List<FoodCommonEntity> findByDeletedIsFalseAndName(String name);

	List<FoodCommonEntity> findByIdStartingWith(String prefix);
}
