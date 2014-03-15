package org.bosik.diacomp.web.frontend.features.foodbase;

import java.util.List;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import org.bosik.diacomp.web.frontend.common.AuthorizedRestClient;

public class FoodbaseRestClient extends AuthorizedRestClient implements FoodBaseService
{
	public FoodbaseRestClient(AuthService authService, String login, String pass, int apiVersion)
	{
		super(authService, login, pass, apiVersion);
	}

	@Override
	public String add(Versioned<FoodItem> item) throws PersistenceException
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		// TODO Auto-generated method stub

	}

	@Override
	public List<Versioned<FoodItem>> findAll(boolean includeRemoved)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Versioned<FoodItem>> findAny(String filter)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<FoodItem> findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<FoodItem> findById(String guid)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Versioned<FoodItem>> findSysAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void update(Versioned<FoodItem> item) throws NotFoundException, PersistenceException
	{
		// TODO Auto-generated method stub

	}
}
