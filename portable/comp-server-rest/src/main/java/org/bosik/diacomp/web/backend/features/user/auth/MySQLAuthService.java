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

import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.NotActivatedException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.web.backend.features.user.auth.validation.Validator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;

import static org.bosik.merklesync.HashUtils.generateGuid;

@Service
public class MySQLAuthService implements AuthService
{
	@Autowired
	private UserEntityRepository userEntityRepository;

	@Override
	public String register(String userName, String password)
	{
		Validator.validateUserName(userName);
		Validator.validatePassword(password);

		if (userEntityRepository.findByName(userName) != null)
		{
			// TODO: use another DuplicateException
			throw new DuplicateException(userName);
		}

		final String activationKey = generateGuid() + generateGuid();

		UserEntity user = new UserEntity();
		user.setName(userName);
		user.setHashPass(HashUtils.createHash(password));
		user.setActivationKey(activationKey);
		user.setRegistrationDate(new Date());
		userEntityRepository.save(user);

		return activationKey;
	}

	@Override
	public int activate(String activationKey)
	{
		if (activationKey == null || activationKey.isEmpty())
		{
			throw new IllegalArgumentException("Key is empty");
		}

		final UserEntity user = userEntityRepository.findByActivationKey(activationKey);
		if (user == null)
		{
			throw new NotAuthorizedException();
		}

		user.setActivationKey(null);
		userEntityRepository.save(user);
		return user.getId();
	}

	@Override
	public int login(String userName, final String password)
	{
		//		Validator.validateUserName(userName);
		//		Validator.validatePassword(password);

		UserEntity user = userEntityRepository.findByName(userName);
		if (user == null)
		{
			throw new NotAuthorizedException();
		}

		if (user.getActivationKey() != null)
		{
			throw new NotActivatedException("Not activated");
		}

		if (!HashUtils.validatePassword(password, user.getHashPass()))
		{
			throw new NotAuthorizedException();
		}

		user.setLoginDate(new Date());
		userEntityRepository.save(user);
		return user.getId();
	}

	@Override
	public String getNameById(int userId)
	{
		return userEntityRepository.findById(userId).map(UserEntity::getName).orElse(null);
	}

	@Override
	public String buildRestoreKey(final String userName)
	{
		Validator.validateUserName(userName);

		final UserEntity user = userEntityRepository.findByName(userName);
		if (user != null)
		{
			user.setRestoreKey(generateGuid() + generateGuid());
			userEntityRepository.save(user);
			return user.getRestoreKey();
		}
		else
		{
			return null;
		}
	}

	@Override
	public void changePassword(String restoreKey, String newPassword)
	{
		if (restoreKey == null || restoreKey.isEmpty() || restoreKey.length() != 2 * 32)
		{
			throw new IllegalArgumentException("Key is empty or wrong size");
		}

		final UserEntity user = userEntityRepository.findByRestoreKey(restoreKey);
		if (user == null)
		{
			throw new IllegalArgumentException("Wrong key");
		}

		Validator.validatePassword(newPassword);

		user.setHashPass(HashUtils.createHash(newPassword));
		user.setRestoreKey(null);
		user.setActivationKey(null);
		userEntityRepository.save(user);
	}
}
