package org.bosik.diacomp.web.frontend.wicket.dialogs.foodeditor;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.modal.ModalWindow;
import org.apache.wicket.model.Model;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;

public abstract class FoodEditor extends ModalWindow
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

	public void show(AjaxRequestTarget target, Model<Versioned<FoodItem>> model)
	{
		setContent(new FoodEditorContentPanel(getContentId(), model)
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			void onCancel(AjaxRequestTarget target)
			{
				FoodEditor.this.onCancel(target);
			}

			@Override
			void onSelect(AjaxRequestTarget target, Model<Versioned<FoodItem>> model)
			{
				FoodEditor.this.onSelect(target, model);
			}
		});
		super.show(target);
	}

	public abstract void onCancel(AjaxRequestTarget target);

	public abstract void onSelect(AjaxRequestTarget target, Model<Versioned<FoodItem>> model);
}
