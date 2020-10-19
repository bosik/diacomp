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

import org.springframework.data.repository.CrudRepository;

import java.util.Date;
import java.util.List;

public interface FoodUserEntityRepository extends CrudRepository<FoodUserEntity, String>
{
	int countByKeyUserId(int userId);

	int countByKeyUserIdAndKeyIdStartingWith(int userId, String prefix);

	List<FoodUserEntity> findByKeyUserId(int userId);

	List<FoodUserEntity> findByKeyUserIdAndDeletedIsFalse(int userId);

	List<FoodUserEntity> findByKeyUserIdAndDeletedIsFalseAndNameContaining(int userId, String filter);

	List<FoodUserEntity> findByKeyUserIdAndDeletedIsFalseAndName(int userId, String exactName);

	List<FoodUserEntity> findByKeyUserIdAndName(int userId, String exactName);

	FoodUserEntity findByKeyUserIdAndKeyId(int userId, String id);

	List<FoodUserEntity> findByKeyUserIdAndKeyIdStartingWith(int userId, String prefix);

	List<FoodUserEntity> findByKeyUserIdAndLastModifiedIsGreaterThanEqual(int userId, Date since);
}
