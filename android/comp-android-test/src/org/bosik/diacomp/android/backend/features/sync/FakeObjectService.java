package org.bosik.diacomp.android.backend.features.sync;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;

public class FakeObjectService implements ObjectService<String>
{
	private final List<Versioned<String>>	data	= new ArrayList<Versioned<String>>();

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		for (Versioned<String> item : data)
		{
			if (item.getId().equals(id))
			{
				if (!item.isDeleted())
				{
					item.setDeleted(true);
					return;
				}
				else
				{
					throw new AlreadyDeletedException(id);
				}
			}
		}

		throw new NotFoundException(id);
	}

	@Override
	public Versioned<String> findById(String id) throws CommonServiceException
	{
		for (Versioned<String> item : data)
		{
			if (item.getId().equals(id))
			{
				return new Versioned<String>(item);
			}
		}
		throw new NotFoundException(id);
	}

	@Override
	public List<Versioned<String>> findChanged(Date since) throws CommonServiceException
	{
		List<Versioned<String>> result = new ArrayList<Versioned<String>>();

		for (Versioned<String> item : data)
		{
			if (item.getTimeStamp().after(since))
			{
				result.add(new Versioned<String>(item));
			}
		}

		return result;
	}

	@Override
	public void save(List<Versioned<String>> items) throws CommonServiceException
	{
		for (Versioned<String> item : items)
		{
			Versioned<String> temp = findById(item.getId());

			if (temp == null)
			{
				data.add(new Versioned<String>(item));
			}
			else
			{
				for (Versioned<String> x : data)
				{
					if (x.getId().equals(item.getId()))
					{
						x.setData(item.getData());
						x.setDeleted(item.isDeleted());
						x.setTimeStamp(item.getTimeStamp());
						x.setVersion(item.getVersion());
						break;
					}
				}

			}
		}
	}
}