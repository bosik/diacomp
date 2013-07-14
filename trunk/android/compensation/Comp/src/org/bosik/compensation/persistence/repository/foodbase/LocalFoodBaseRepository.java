package org.bosik.compensation.persistence.repository.foodbase;

import org.bosik.compensation.persistence.entity.foodbase.FoodBase;
import org.bosik.compensation.persistence.repository.common.BaseRepository;
import org.bosik.compensation.persistence.repository.common.LocalBaseRepository;
import android.content.Context;

public class LocalFoodBaseRepository extends LocalBaseRepository<FoodBase, FoodBaseXMLFormatter> implements
		BaseRepository<FoodBase>
{
	public LocalFoodBaseRepository(String fileName, Context context, FoodBaseXMLFormatter formatter)
	{
		super(fileName, context, formatter);
	}
}