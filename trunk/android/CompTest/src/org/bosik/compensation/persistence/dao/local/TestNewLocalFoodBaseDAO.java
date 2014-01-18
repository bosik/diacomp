package org.bosik.compensation.persistence.dao.local;

import org.bosik.compensation.persistence.dao.TestFoodBaseDAO;
import org.bosik.diacomp.persistence.dao.FoodBaseDAO;

public class TestNewLocalFoodBaseDAO extends TestFoodBaseDAO
{
	@Override
	protected FoodBaseDAO getDAO()
	{
		// assertNotNull(getContext());
		// ContentResolver resolver = getContext().getContentResolver();
		// return new NewLocalFoodBaseDAO(resolver);
		return null;
	}
}