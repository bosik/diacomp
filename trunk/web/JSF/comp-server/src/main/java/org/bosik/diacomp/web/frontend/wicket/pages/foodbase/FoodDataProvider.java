package org.bosik.diacomp.web.frontend.wicket.pages.foodbase;

import java.util.Iterator;
import java.util.List;
import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import org.bosik.diacomp.web.frontend.features.auth.AuthRestClient;
import org.bosik.diacomp.web.frontend.features.foodbase.FoodbaseRestClient;

public class FoodDataProvider implements IDataProvider<Versioned<FoodItem>>
{
	private static final String			USER_NAME			= "bosik-007@narod.ru";
	private static final String			PASSWORD			= "devel0pment";
	private static final int			API_VERSION			= 20;

	private static final AuthService	authService			= new AuthRestClient();
	public static final FoodBaseService	foodService			= new FoodbaseRestClient(authService, USER_NAME, PASSWORD,
																	API_VERSION);

	private static final long			serialVersionUID	= 1L;

	@Override
	public void detach()
	{
	}

	@Override
	public Iterator<? extends Versioned<FoodItem>> iterator(long first, long count)
	{
		int iFirst = (int)first;
		int iCount = (int)count;

		List<Versioned<FoodItem>> items = foodService.findAll(false);
		return items./* subList(iFirst, iFirst + iCount). */iterator();
	}

	@Override
	public long size()
	{
		return foodService.findAll(false).size();
	}

	@Override
	public IModel<Versioned<FoodItem>> model(Versioned<FoodItem> object)
	{
		return new DetachableFoodModel(object);
	}
}
