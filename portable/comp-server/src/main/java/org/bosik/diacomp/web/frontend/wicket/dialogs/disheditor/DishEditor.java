package org.bosik.diacomp.web.frontend.wicket.dialogs.disheditor;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.web.frontend.wicket.dialogs.common.CommonEditor;

public abstract class DishEditor extends CommonEditor<DishItem>
{
	private static final long	serialVersionUID	= 1L;

	public DishEditor(String id)
	{
		super(id);

		setMinimalWidth(480);
		setMinimalHeight(580);

		setInitialWidth(480);
		setInitialHeight(580);

		setTitle(getString("dishEditor.caption"));
	}

	public void show(AjaxRequestTarget target, IModel<Versioned<DishItem>> model)
	{
		setContent(new DishEditorContentPanel(getContentId(), model)
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected void onCancel(AjaxRequestTarget target)
			{
				DishEditor.this.onCancel(target);
			}

			@Override
			protected void onSave(AjaxRequestTarget target, IModel<Versioned<DishItem>> model)
			{
				DishEditor.this.onSave(target, model);
			}
		});
		super.show(target);
	}
}
