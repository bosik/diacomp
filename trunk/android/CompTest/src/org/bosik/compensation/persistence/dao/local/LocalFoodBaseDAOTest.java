package org.bosik.compensation.persistence.dao.local;

import org.bosik.compensation.persistence.dao.FoodBaseDAO;
import org.bosik.compensation.persistence.dao.FoodBaseDAOTest;

public class LocalFoodBaseDAOTest extends FoodBaseDAOTest
{
	@Override
	protected FoodBaseDAO getDAO()
	{
		// FIXME
		assertNotNull(getContext());
		return new LocalFoodBaseDAO();
	}
}