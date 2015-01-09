package org.bosik.diacomp.web.backend.features.base.food.service;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.web.backend.features.auth.service.AuthService;
import org.bosik.diacomp.web.backend.features.auth.service.FrontendAuthService;
import org.bosik.diacomp.web.backend.features.base.food.function.FoodbaseDAO;
import org.bosik.diacomp.web.backend.features.base.food.function.MySQLFoodbaseDAO;
import org.springframework.stereotype.Service;

@Service
public class FrontendFoodbaseService implements FoodBaseService
{
	private final AuthService	authService	= new FrontendAuthService();
	private final FoodbaseDAO	foodbaseDao	= new MySQLFoodbaseDAO();

	@Override
	public void add(Versioned<FoodItem> item) throws DuplicateException, PersistenceException
	{
		int userId = authService.getCurrentUserId();
		foodbaseDao.save(userId, Arrays.<Versioned<FoodItem>> asList(item));
	}

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		int userId = authService.getCurrentUserId();
		foodbaseDao.delete(userId, id);
	}

	@Override
	public List<Versioned<FoodItem>> findAll(boolean includeRemoved)
	{
		int userId = authService.getCurrentUserId();
		return foodbaseDao.findAll(userId, includeRemoved);
	}

	@Override
	public List<Versioned<FoodItem>> findAny(String filter)
	{
		int userId = authService.getCurrentUserId();
		return foodbaseDao.findAny(userId, filter);
	}

	@Override
	public Versioned<FoodItem> findById(String id) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		return foodbaseDao.findById(userId, id);
	}

	@Override
	public List<Versioned<FoodItem>> findByIdPrefix(String prefix) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		return foodbaseDao.findByIdPrefix(userId, prefix);
	}

	@Override
	public List<Versioned<FoodItem>> findChanged(Date since) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		return foodbaseDao.findChanged(userId, since);
	}

	@Override
	public Versioned<FoodItem> findOne(String exactName)
	{
		int userId = authService.getCurrentUserId();
		return foodbaseDao.findOne(userId, exactName);
	}

	@Override
	public String getHash(String prefix) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		return foodbaseDao.getHash(userId, prefix);
	}

	@Override
	public Map<String, String> getHashChildren(String prefix) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		return foodbaseDao.getHashChildren(userId, prefix);
	}

	@Override
	public void save(List<Versioned<FoodItem>> items) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		foodbaseDao.save(userId, items);
	}
}
