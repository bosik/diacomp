package org.bosik.diacomp.web.backend.features.dishbase.rest;

@Deprecated
public class DishbaseRestClient //extends AuthorizedRestClient implements DishBaseService
{
	//	private static final long						serialVersionUID	= 1L;
	//
	//	private final Serializer<Versioned<DishItem>>	serializer			= new SerializerDishItem();
	//
	//	public DishbaseRestClient(AuthService authService, String login, String pass, int apiVersion)
	//	{
	//		super(authService, login, pass, apiVersion);
	//	}
	//
	//	@Override
	//	public void add(Versioned<DishItem> item) throws PersistenceException
	//	{
	//		// TODO: current implementation doesn't fail for duplicates
	//		save(Arrays.asList(item));
	//	}
	//
	//	@Override
	//	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	//	{
	//		Versioned<DishItem> item = findById(id);
	//
	//		if (item == null)
	//		{
	//			throw new NotFoundException(id);
	//		}
	//
	//		if (item.isDeleted())
	//		{
	//			throw new AlreadyDeletedException(id);
	//		}
	//
	//		item.setDeleted(true);
	//		save(Arrays.asList(item));
	//	}
	//
	//	@Override
	//	public List<Versioned<DishItem>> findAll(boolean includeRemoved)
	//	{
	//		try
	//		{
	//			WebResource resource = getResource("api/dish/all");
	//			resource = resource.queryParam("show_rem", Utils.formatBooleanInt(includeRemoved));
	//			String str = authGet(resource);
	//
	//			StdResponse resp = new StdResponse(str);
	//			checkResponse(resp);
	//
	//			return serializer.readAll(resp.getResponse());
	//		}
	//		catch (UniformInterfaceException e)
	//		{
	//			handleUniformInterfaceException(e);
	//			return null; // previous method will throw exception anyway
	//		}
	//	}
	//
	//	@Override
	//	public List<Versioned<DishItem>> findAny(String filter)
	//	{
	//		try
	//		{
	//			WebResource resource = getResource("api/dish/search");
	//			resource = resource.queryParam("q", filter);
	//			String str = authGet(resource);
	//
	//			StdResponse resp = new StdResponse(str);
	//			checkResponse(resp);
	//
	//			return serializer.readAll(resp.getResponse());
	//		}
	//		catch (UniformInterfaceException e)
	//		{
	//			handleUniformInterfaceException(e);
	//			return null; // previous method will throw exception anyway
	//		}
	//	}
	//
	//	@Override
	//	public Versioned<DishItem> findOne(String exactName)
	//	{
	//		// TODO Auto-generated method stub
	//		return null;
	//	}
	//
	//	@Override
	//	public Versioned<DishItem> findById(String id)
	//	{
	//		try
	//		{
	//			WebResource resource = getResource(String.format("api/dish/guid/%s", id));
	//			String str = authGet(resource);
	//
	//			StdResponse resp = new StdResponse(str);
	//			checkResponse(resp);
	//
	//			return resp.getCode() != ResponseBuilder.CODE_NOTFOUND ? serializer.read(resp.getResponse()) : null;
	//		}
	//		catch (UniformInterfaceException e)
	//		{
	//			handleUniformInterfaceException(e);
	//			return null; // previous method will throw exception anyway
	//		}
	//	}
	//
	//	@Override
	//	public List<Versioned<DishItem>> findChanged(Date since)
	//	{
	//		try
	//		{
	//			WebResource resource = getResource("api/dish/changes");
	//			resource = resource.queryParam("since", Utils.formatTimeUTC(since));
	//
	//			String str = authGet(resource);
	//			StdResponse resp = new StdResponse(str);
	//			checkResponse(resp);
	//
	//			return serializer.readAll(resp.getResponse());
	//		}
	//		catch (UniformInterfaceException e)
	//		{
	//			handleUniformInterfaceException(e);
	//			return null; // previous method will throw exception anyway
	//		}
	//	}
	//
	//	@Override
	//	public void save(List<Versioned<DishItem>> items) throws NotFoundException, PersistenceException
	//	{
	//		WebResource resource = getResource("api/dish/");
	//		try
	//		{
	//			Form form = new Form();
	//			form.add("items", serializer.writeAll(items));
	//			String str = authPut(resource, form);
	//
	//			StdResponse resp = new StdResponse(str);
	//			checkResponse(resp);
	//		}
	//		catch (UniformInterfaceException e)
	//		{
	//			System.err.println(e.getResponse().getEntity(String.class));
	//			handleUniformInterfaceException(e);
	//		}
	//	}
}
