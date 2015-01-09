package org.bosik.diacomp.web.frontend.wicket.pages.base;

import java.util.Iterator;
import java.util.List;
import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.base.food.FoodBaseService;

public class FoodDataProvider implements IDataProvider<Versioned<FoodItem>>
{
	private static final long	serialVersionUID	= 1L;

	@SpringBean
	public FoodBaseService		foodService;

	@Override
	public void detach()
	{
	}

	@Override
	public Iterator<? extends Versioned<FoodItem>> iterator(long first, long count)
	{
		List<Versioned<FoodItem>> items = foodService.findAll(false);

		int iFirst = (int)first;
		int iLast = (int)(first + count);

		if (first < 0)
		{
			first = 0;
		}
		if (iLast > items.size())
		{
			iLast = items.size();
		}

		return items.subList(iFirst, iLast).iterator();
	}

	@Override
	public long size()
	{
		return foodService.findAll(false).size();
	}

	@Override
	public IModel<Versioned<FoodItem>> model(Versioned<FoodItem> object)
	{
		return Model.of(object);
	}
}
