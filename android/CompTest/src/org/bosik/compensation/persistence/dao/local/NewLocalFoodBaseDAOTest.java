package org.bosik.compensation.persistence.dao.local;

import org.bosik.compensation.persistence.dao.FoodBaseDAO;
import org.bosik.compensation.persistence.dao.FoodBaseDAOTest;
import android.content.ContentResolver;

public class NewLocalFoodBaseDAOTest extends FoodBaseDAOTest
{
	@Override
	protected FoodBaseDAO getDAO()
	{
		assertNotNull(getContext());
		ContentResolver resolver = getContext().getContentResolver();
		return new NewLocalFoodBaseDAO(resolver);
	}
}