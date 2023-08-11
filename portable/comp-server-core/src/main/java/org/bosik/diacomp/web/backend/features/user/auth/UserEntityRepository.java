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
package org.bosik.diacomp.web.backend.features.user.auth;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface UserEntityRepository extends CrudRepository<UserEntity, Integer>
{
	UserEntity findByName(String name);

	UserEntity findByActivationKey(String activationKey);

	UserEntity findByRestoreKey(String restoreKey);

	@Query("SELECT u FROM UserEntity u WHERE u.deletionDate IS NOT NULL AND u.deletionDate < NOW() AND u.loginDeleted IS NULL")
	List<UserEntity> findUsersToCleanup();
}
