package org.bosik.compensation.persistence.dao.local;

import org.bosik.compensation.persistence.dao.FoodBaseDAO;
import org.bosik.compensation.persistence.dao.TestFoodBaseDAO;
import org.bosik.diacomp.persistence.dao.local.LocalFoodBaseDAO;
import android.test.suitebuilder.annotation.Suppress;

@Suppress
public class TestLocalFoodBaseDAO extends TestFoodBaseDAO
{
	@Override
	protected FoodBaseDAO getDAO()
	{
		// FIXME
		assertNotNull(getContext());
		return new LocalFoodBaseDAO();
	}
}