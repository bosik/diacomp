package org.bosik.diacomp.web.backend.features.foodbase.service;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import org.bosik.diacomp.web.backend.features.auth.service.AuthService;
import org.bosik.diacomp.web.backend.features.auth.service.FrontendAuthService;
import org.bosik.diacomp.web.backend.features.foodbase.function.FakeFoodbaseDAO;
import org.bosik.diacomp.web.backend.features.foodbase.function.FoodbaseDAO;

public class FrontendFoodbaseService implements FoodBaseService
{
	private final AuthService	authService	= new FrontendAuthService();
	private final FoodbaseDAO	foodbaseDao	= new FakeFoodbaseDAO();

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
	public void save(List<Versioned<FoodItem>> items) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		foodbaseDao.save(userId, items);
	}
}
