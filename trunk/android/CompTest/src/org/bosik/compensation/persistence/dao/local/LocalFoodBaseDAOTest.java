package org.bosik.compensation.persistence.dao.local;

import java.io.IOException;
import org.bosik.compensation.persistence.dao.FoodBaseDAO;
import org.bosik.compensation.persistence.dao.FoodBaseDAOTest;

public class LocalFoodBaseDAOTest extends FoodBaseDAOTest
{
	@Override
	protected FoodBaseDAO getDAO()
	{
		assertNotNull(getContext());
		try
		{
			return new LocalFoodBaseDAO(getContext(), "testFoodBase.xml");
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}
}