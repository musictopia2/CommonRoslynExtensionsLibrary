using CommonBasicLibraries.AdvancedGeneralFunctionsAndProcesses.BasicExtensions;
using CommonBasicLibraries.CollectionClasses;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
namespace CommonRoslynExtensionsLibrary;
public static class Extensions
{
    public static INamedTypeSymbol GetClassSymbol(this Compilation compilation, ClassDeclarationSyntax clazz)
    {
        var model = compilation.GetSemanticModel(clazz.SyntaxTree);
        var classSymbol = model.GetDeclaredSymbol(clazz)!;
        return classSymbol;
    }
    public static INamedTypeSymbol GetRecordSymbol(this Compilation compilation, RecordDeclarationSyntax record)
    {
        var model = compilation.GetSemanticModel(record.SyntaxTree);
        var recordSymbol = model.GetDeclaredSymbol(record)!;
        return recordSymbol;
    }
    public static INamedTypeSymbol GetStructSymbol(this Compilation compilation, StructDeclarationSyntax struz)
    {
        var model = compilation.GetSemanticModel(struz.SyntaxTree);
        var structSymbol = model.GetDeclaredSymbol(struz)!;
        return structSymbol;
    }
    public static string GetAccessModifier(this INamedTypeSymbol symbol)
    {
        return symbol.DeclaredAccessibility.ToString().ToLowerInvariant();
    }
    public static bool IsPartial(this ClassDeclarationSyntax classDeclaration)
    {
        return classDeclaration.Modifiers.Any(m => m.IsKind(SyntaxKind.PartialKeyword));
    }
    public static bool IsPartial(this RecordDeclarationSyntax recordDeclaration)
    {
        return recordDeclaration.Modifiers.Any(m => m.IsKind(SyntaxKind.PartialKeyword));
    }
    public static bool IsPartial(this StructDeclarationSyntax structDeclaration)
    {
        return structDeclaration.Modifiers.Any(m => m.IsKind(SyntaxKind.PartialKeyword));
    }
    public static bool IsMappable(this ClassDeclarationSyntax source) => source.Implements("IMappable");
    public static bool Implements(this ClassDeclarationSyntax source, string interfaceName)
    {
        if (source.BaseList is null)
        {
            return false;
        }
        IEnumerable<BaseTypeSyntax> baseTypes = source.BaseList.Types.Select(baseType => baseType);
        return baseTypes.Any(baseType => baseType.ToString() == interfaceName);
    }
    public static bool IsMappable(this RecordDeclarationSyntax source) => source.Implements("IMappable");
    public static bool Implements(this RecordDeclarationSyntax source, string interfaceName)
    {
        if (source.BaseList is null)
        {
            return false;
        }
        IEnumerable<BaseTypeSyntax> baseTypes = source.BaseList.Types.Select(baseType => baseType);
        return baseTypes.Any(baseType => baseType.ToString() == interfaceName);
    }
    public static bool IsMappable(this StructDeclarationSyntax source) => source.Implements("IMappable");
    public static bool Implements(this StructDeclarationSyntax source, string interfaceName)
    {
        if (source.BaseList is null)
        {
            return false;
        }
        IEnumerable<BaseTypeSyntax> baseTypes = source.BaseList.Types.Select(baseType => baseType);
        return baseTypes.Any(baseType => baseType.ToString() == interfaceName);
    }
    public static bool IsMappable(this ITypeSymbol symbol) => symbol.Implements("IMappable");
    public static bool Implements(this ITypeSymbol symbol, string interfaceName)
    {
        var firsts = symbol.AllInterfaces;
        return firsts.Any(xx => xx.Name == interfaceName);
    }
    public static bool IsCollection(this ITypeSymbol symbol)
    {
        bool rets = symbol.Implements("ICollection");
        return rets;
    }
    public static bool IsCollection(this IPropertySymbol symbol)
    {
        return symbol.Type.IsCollection();
    }
    public static ITypeSymbol? GetCollectionSingleGenericTypeUsed(this ITypeSymbol symbol)
    {
        INamedTypeSymbol? others = symbol as INamedTypeSymbol;
        if (others is null)
        {
            return null;
        }
        if (others.TypeArguments.Count() is not 1)
        {
            return null;
        }
        return others.TypeArguments[0];
    }
    public static ITypeSymbol? GetCollectionSingleGenericTypeUsed(this IPropertySymbol symbol)
    {
        return symbol.Type.GetCollectionSingleGenericTypeUsed();
    }
    public static bool IsSimpleType(this ITypeSymbol symbol)
    {
        if (symbol.Name == "String")
        {
            return true;
        }
        if (symbol.Name == "Nullable")
        {
            return true;
        }
        if (symbol.TypeKind == TypeKind.Enum)
        {
            return true;
        }
        if (symbol.TypeKind == TypeKind.Struct)
        {
            return true;
        }
        return false;
    }
    public static bool IsSimpleType(this IPropertySymbol symbol)
    {
        return symbol.Type.IsSimpleType();
    }
    public static BasicList<IPropertySymbol> GetProperties(this INamedTypeSymbol symbol) => symbol.GetMembers().OfType<IPropertySymbol>().ToBasicList();
    public static BasicList<IPropertySymbol> GetProperties(this INamedTypeSymbol symbol, Func<IPropertySymbol, bool> predicate) => symbol.GetMembers().OfType<IPropertySymbol>().Where(predicate).ToBasicList();
    public static bool TryGetAttribute(this ISymbol symbol, string attributeName, out IEnumerable<AttributeData> attributes)
    {
        string otherName = attributeName.Replace("Attribute", "");
        attributes = symbol.GetAttributes()
            .Where(a => a.AttributeClass is not null && (a.AttributeClass.Name == attributeName || a.AttributeClass.Name == otherName));
        return attributes.Any();
    }
    public static bool TryGetAttribute(this ISymbol symbol, INamedTypeSymbol attributeType, out IEnumerable<AttributeData> attributes)
    {
        attributes = symbol.GetAttributes()
            .Where(a => SymbolEqualityComparer.Default.Equals(a.AttributeClass, attributeType));
        return attributes.Any();
    }
    public static bool HasAttribute(this ISymbol symbol, INamedTypeSymbol attributeType)
    {
        return symbol.GetAttributes().Any(a => SymbolEqualityComparer.Default.Equals(a.AttributeClass, attributeType));
    }
    public static bool HasAttribute(this ISymbol symbol, string attributeName)
    {
        string otherName = attributeName.Replace("Attribute", "");
        return symbol.GetAttributes()
            .Any(a => a.AttributeClass is not null && (a.AttributeClass.Name == attributeName || a.AttributeClass.Name == otherName));
    }
    public static bool AttributePropertyUsed(this IEnumerable<AttributeData> attributes, string propertyName)
    {
        AttributeData attribute = attributes.Single();
        return attribute.AttributePropertyUsed(propertyName);
    }
    public static bool AttributePropertyUsed(this AttributeData attribute, string propertyName)
    {
        bool? output = (bool?)attribute.NamedArguments.FirstOrDefault(xx => xx.Key.Equals(propertyName)).Value.Value;
        if (output is null)
        {
            return false;
        }
        return output.Value;
    }
    public static int? AttributePropertyOptionChosen(this IEnumerable<AttributeData> attributes, string propertyName)
    {
        AttributeData attribute = attributes.Single();
        return attribute.AttributePropertyOptionChosen(propertyName);
    }
    public static int? AttributePropertyOptionChosen(this AttributeData attribute, string propertyName)
    {
        int? output = (int?)attribute.NamedArguments.FirstOrDefault(xx => xx.Key.Equals(propertyName)).Value.Value;
        if (output is null)
        {
            return null;
        }
        return output.Value;
    }
    public static T? AttributePropertyOptionData<T>(this IEnumerable<AttributeData> attributes, string propertyName)
    {
        AttributeData attribute = attributes.Single();
        return attribute.AttributePropertyOptionData<T>(propertyName);
    }
    public static T? AttributePropertyOptionData<T>(this AttributeData attribute, string propertyName)
    {
        T? output = (T?)attribute.NamedArguments.FirstOrDefault(xx => xx.Key.Equals(propertyName)).Value.Value;
        if (output is null)
        {
            return default;
        }
        return output;
    }
    /// <summary>
    /// this is used in order to make the string compatible with c#.  since this is intended to be used for source generation.
    /// often times, will put this data into the code file itself
    /// </summary>
    /// <param name="content"></param>
    /// <returns></returns>
    public static string GetCSharpString(this string content)
    {
        content = content.Replace("\"", "\"\"");
        return content;
    }
#pragma warning disable RS2008 // Enable analyzer release tracking
    public static DiagnosticDescriptor ReportError(this string errorMessage, string id) => new (id,
#pragma warning restore RS2008 // Enable analyzer release tracking
        "Could not create source generation",
        errorMessage,
        "Error",
        DiagnosticSeverity.Error,
        true
        );
}