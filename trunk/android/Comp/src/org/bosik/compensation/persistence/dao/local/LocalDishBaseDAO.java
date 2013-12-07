package org.bosik.compensation.persistence.dao.local;

import java.io.IOException;
import org.bosik.compensation.bo.dishbase.DishItem;
import org.bosik.compensation.persistence.dao.DishBaseDAO;
import org.bosik.compensation.persistence.dao.local.utils.FileBaseDAO;
import org.bosik.compensation.persistence.serializers.dishbase.DishBaseXMLSerializer;
import android.content.Context;

public class LocalDishBaseDAO extends FileBaseDAO<DishItem> implements DishBaseDAO
{
	public LocalDishBaseDAO(Context context, String fileName) throws IOException
	{
		// hide last parameter with created serializer
		super(context, fileName, new DishBaseXMLSerializer());
	}
}