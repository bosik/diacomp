package org.bosik.diacomp.web.frontend.wicket.dialogs.common;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxFallbackButton;
import org.apache.wicket.markup.html.IHeaderContributor;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.bosik.diacomp.core.entities.tech.Versioned;

public abstract class CommonEditorContentPanel<T> extends Panel implements IHeaderContributor
{
	private static final long		serialVersionUID	= 1L;

	protected Form<Void>			form;
	protected IModel<Versioned<T>>	model;

	public CommonEditorContentPanel(String id, final IModel<Versioned<T>> model)
	{
		super(id);
		this.model = model;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		form = new Form<Void>("form");
		add(form);

		form.add(new AjaxFallbackButton("buttonSave", form)
		{
			private static final long	serialVersionUID	= 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> form)
			{
				onSave(target, model);
			}
		});

		form.add(new AjaxFallbackButton("buttonCancel", form)
		{
			private static final long	serialVersionUID	= -3966833383602736092L;

			@Override
			public void onSubmit(AjaxRequestTarget target, Form<?> form)
			{
				onCancel(target);
			}
		});
	}

	protected abstract void onSave(AjaxRequestTarget target, IModel<Versioned<T>> model);

	protected abstract void onCancel(AjaxRequestTarget target);
}
