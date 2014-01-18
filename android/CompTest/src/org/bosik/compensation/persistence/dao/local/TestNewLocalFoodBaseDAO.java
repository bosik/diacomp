package org.bosik.compensation.persistence.dao.local;

import org.bosik.compensation.persistence.dao.FoodBaseDAO;
import org.bosik.compensation.persistence.dao.TestFoodBaseDAO;
import org.bosik.diacomp.persistence.dao.local.NewLocalFoodBaseDAO;
import android.content.ContentResolver;

public class TestNewLocalFoodBaseDAO extends TestFoodBaseDAO
{
	@Override
	protected FoodBaseDAO getDAO()
	{
		assertNotNull(getContext());
		ContentResolver resolver = getContext().getContentResolver();
		return new NewLocalFoodBaseDAO(resolver);
	}
}