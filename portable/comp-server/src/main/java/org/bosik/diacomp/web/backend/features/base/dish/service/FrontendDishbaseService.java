package org.bosik.diacomp.web.backend.features.base.dish.service;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.web.backend.features.auth.service.AuthService;
import org.bosik.diacomp.web.backend.features.auth.service.FrontendAuthService;
import org.bosik.diacomp.web.backend.features.base.dish.function.DishbaseDAO;
import org.bosik.diacomp.web.backend.features.base.dish.function.MySQLDishbaseDAO;

public class FrontendDishbaseService implements DishBaseService
{
	private final AuthService	authService	= new FrontendAuthService();
	private final DishbaseDAO	dishbaseDao	= new MySQLDishbaseDAO();

	@Override
	public void add(Versioned<DishItem> item) throws DuplicateException, PersistenceException
	{
		int userId = authService.getCurrentUserId();
		dishbaseDao.save(userId, Arrays.<Versioned<DishItem>> asList(item));
	}

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		int userId = authService.getCurrentUserId();
		dishbaseDao.delete(userId, id);
	}

	@Override
	public List<Versioned<DishItem>> findAll(boolean includeRemoved)
	{
		int userId = authService.getCurrentUserId();
		return dishbaseDao.findAll(userId, includeRemoved);
	}

	@Override
	public List<Versioned<DishItem>> findAny(String filter)
	{
		int userId = authService.getCurrentUserId();
		return dishbaseDao.findAny(userId, filter);
	}

	@Override
	public Versioned<DishItem> findById(String id) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		return dishbaseDao.findById(userId, id);
	}

	@Override
	public List<Versioned<DishItem>> findByIdPrefix(String prefix) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		return dishbaseDao.findByIdPrefix(userId, prefix);
	}

	@Override
	public List<Versioned<DishItem>> findChanged(Date since) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		return dishbaseDao.findChanged(userId, since);
	}

	@Override
	public Versioned<DishItem> findOne(String exactName)
	{
		int userId = authService.getCurrentUserId();
		return dishbaseDao.findOne(userId, exactName);
	}

	@Override
	public String getHash(String prefix) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		return dishbaseDao.getHash(userId, prefix);
	}

	@Override
	public void save(List<Versioned<DishItem>> items) throws CommonServiceException
	{
		int userId = authService.getCurrentUserId();
		dishbaseDao.save(userId, items);
	}
}
