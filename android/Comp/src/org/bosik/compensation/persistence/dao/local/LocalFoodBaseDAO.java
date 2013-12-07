package org.bosik.compensation.persistence.dao.local;

import java.io.IOException;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.dao.local.utils.FileBaseDAO;
import org.bosik.compensation.persistence.serializers.foodbase.FoodBaseXMLSerializer;
import android.content.Context;

public class LocalFoodBaseDAO extends FileBaseDAO<FoodItem>
{
	public LocalFoodBaseDAO(Context context, String fileName) throws IOException
	{
		// hide last parameter with created serializer
		super(context, fileName, new FoodBaseXMLSerializer());
	}
}