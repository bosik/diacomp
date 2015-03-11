package org.bosik.diacomp.web.frontend.wicket.dialogs.foodeditor;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.web.frontend.wicket.dialogs.common.CommonEditor;

public abstract class FoodEditor extends CommonEditor<FoodItem>
{
	private static final long	serialVersionUID	= 1L;

	public FoodEditor(String id)
	{
		super(id);

		setMinimalWidth(350);
		setMinimalHeight(350);

		setInitialWidth(350);
		setInitialHeight(350);

		setTitle(getString("foodEditor.caption"));
	}

	public void show(AjaxRequestTarget target, IModel<Versioned<FoodItem>> model)
	{
		setContent(new FoodEditorContentPanel(getContentId(), model)
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected void onCancel(AjaxRequestTarget target)
			{
				FoodEditor.this.onCancel(target);
			}

			@Override
			protected void onSave(AjaxRequestTarget target, IModel<Versioned<FoodItem>> model)
			{
				FoodEditor.this.onSave(target, model);
			}
		});
		super.show(target);
	}
}
