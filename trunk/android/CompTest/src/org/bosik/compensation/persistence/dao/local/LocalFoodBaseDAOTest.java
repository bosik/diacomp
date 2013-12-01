package org.bosik.compensation.persistence.dao.local;

import java.io.IOException;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.dao.BaseDAO;
import org.bosik.compensation.persistence.dao.FoodBaseDAOTest;

public class LocalFoodBaseDAOTest extends FoodBaseDAOTest
{
	@Override
	protected BaseDAO<FoodItem> getDAO()
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