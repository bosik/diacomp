package org.bosik.diacomp.android.backend.features.foodbase;

import java.util.List;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.foodbase.FoodBaseService;

public class FoodbaseRestClient implements FoodBaseService
{
	// private static final String TAG = FoodbaseRestClient.class.getSimpleName();

	private final WebClient							webClient;
	private final Serializer<Versioned<FoodItem>>	serializer;

	public FoodbaseRestClient(WebClient webClient, Serializer<Versioned<FoodItem>> serializer)
	{
		this.webClient = webClient;
		this.serializer = serializer;
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
	public void save(List<Versioned<FoodItem>> items) throws NotFoundException, PersistenceException
	{
		// TODO Auto-generated method stub

	}

}
